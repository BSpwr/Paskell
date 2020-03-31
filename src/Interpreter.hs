module Interpreter where

import           Control.Monad                  ( void
                                                , when
                                                , unless
                                                )
import           Control.Monad.State
import           Control.Monad.IO.Class

import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Stack                     ( Stack )
import qualified Data.Stack                    as Stack
import           Data.Fixed
import           Data.Bits
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , toLower
                                                )
import           Data.Maybe                     ( maybe
                                                , Maybe(..)
                                                , isNothing
                                                , fromJust
                                                , fromMaybe
                                                )

import           TypeAST
import           TypeValue

type PaskellState = StateT InterpreterState IO

type InterpreterState = (ConstTable, VarTable, FuncTable)

type ConstTable = Map Text Value
type VarTable = Map Text (VarType, Maybe Value)
type FuncTable = Map Text Function

interpreterRun :: AST -> IO InterpreterState
interpreterRun ast =
    execStateT (interpreterStart ast) (Map.empty, Map.empty, Map.empty)

interpreterStart :: AST -> PaskellState ()
interpreterStart (Node "Root" a b) = do
    interpret a
    interpret b

readValue :: VarType -> IO Value
readValue vt = case vt of
    BoolType   -> VBool <$> readBool
    IntType    -> VInt <$> readInt
    RealType   -> VDouble <$> readDouble
    StringType -> VString . pack <$> readString
    EnumType s -> VEnum . pack <$> readString

readBool :: IO Bool
readBool = readLn
readInt :: IO Int
readInt = readLn
readDouble :: IO Double
readDouble = readLn
readString :: IO String
readString = getLine

interpret :: AST -> PaskellState ()
interpret (Block     blocks) = mapM_ execBlock blocks
interpret (ProgBlock sb    ) = execStatement sb

execBlock :: BlockDef -> PaskellState ()
execBlock (VarBlock   vds   ) = mapM_ varDef vds
execBlock (ConstBlock consts) = mapM_ constDef consts
execBlock (FuncBlock  funcs ) = mapM_ funcDef funcs

execStatement :: Statement -> PaskellState ()
execStatement (Assign (t, expr)) = do
    v <- evalExpr expr
    doAssign (t, Just v)

execStatement (Writeln exprs) = do
    vs <- mapM evalExpr exprs
    mapM_ printValue vs
    liftIO $ putStrLn ""

execStatement (Readln varNames       ) = mapM_ storeValueFromStdin varNames

execStatement (StatementIf expr s1 s2) = do
    ev <- evalExpr expr
    case ev of
        VBool b -> if b
            then void $ execStatement s1
            else case s2 of
                Just s  -> void $ execStatement s
                Nothing -> return ()
        _ ->
            error
                $ "Expecting boolean type expression for if statement, instead received ->"
                ++ show ev
                ++ "<-"

execStatement (StatementBlock sb            ) = mapM_ execStatement sb

execStatement (StatementWhile expr statement) = do
    v <- evalExpr expr
    case v of
        VBool b ->
            when b
                $  execStatement statement
                >> (execStatement (StatementWhile expr statement))
        _ -> error "Expression for while loop must be boolean type"

execStatement (StatementFor (t, ei) loopDir ef stat) = do
    vi <- evalExpr ei
    vf <- evalExpr ef
    case (vi, vf) of
        (VInt i, VInt f) -> execForLoop stat (t, i, loopDir, f)
        _                -> error "For loop variable must be integer type"

execStatement (StatementRepeatUntil sb expr) = do
    execStatement $ StatementBlock sb
    v <- evalExpr expr
    case v of
        VBool b -> unless b $ execStatement $ StatementRepeatUntil sb expr
        _       -> error "Expression for repeat until loop must be boolean type"

execStatement (StatementCase expr cls ms) = do
    exprE <- evalExpr expr
    b     <- execCaseLines exprE cls
    unless b $ case ms of
        Just s  -> void $ execStatement s
        Nothing -> return ()

execStatement (ProcCall name exprs) = execFunc name exprs >> return ()

execForLoop
    :: Statement -> (Text, Int, ForLoopDirection, Int) -> PaskellState ()
execForLoop stat (t, c, loopDir, f) = do
    let (status, next) = case loopDir of
            To     -> (c <= f, c + 1)
            DownTo -> (c >= f, c - 1)
    when status
        $  doAssign (t, Just $ VInt c)
        >> execStatement stat
        >> (execForLoop stat (t, next, loopDir, f))

execCaseLines :: Value -> [CaseLine] -> PaskellState Bool
execCaseLines val (cl : cls) = do
    b <- execCaseLine val cl
    if b then return True else execCaseLines val cls

execCaseLines val [] = return False

execCaseLine :: Value -> CaseLine -> PaskellState Bool
execCaseLine val (CaseLine vls stat) = if checkIfExistInValueList val vls
    then execStatement stat >> return True
    else return False

checkIfExistInValueList :: Value -> [ValueLiteral] -> Bool
checkIfExistInValueList val vls = val `elem` vs
    where vs = map evalValueLiteral vls

storeValueFromStdin :: Text -> PaskellState ()
storeValueFromStdin t = do
    (constTable, varTable, funcTable) <- get
    (vt, mv) <- varGet t
    v        <- liftIO $ readValue vt
    put (constTable, Map.insert t (vt, Just v) varTable, funcTable)

printValue :: Value -> PaskellState ()
printValue v = liftIO $ putStr $ showValue v

showValue :: Value -> String
showValue (VBool   b) = show b
showValue (VInt    i) = show i
showValue (VDouble d) = show d
showValue (VString s) = unpack s
showValue (VEnum   e) = unpack e

funcDef :: Function -> PaskellState ()
funcDef func@(Function (FunctionDec name param retType) blocks stat) = do
    (constTable, varTable, funcTable) <- get
    put (constTable, varTable, Map.insert name func funcTable)

funcGet :: Text -> PaskellState (Function)
funcGet name = do
    (_, _, funcTable) <- get
    pure $ funcPureGet name funcTable

funcPureGet :: Text -> FuncTable -> Function
funcPureGet name funcTable = do
    let f = Map.lookup name funcTable
    case f of
        Just func -> func
        Nothing ->
            error $ "Function ->" ++ show name ++ "<- was never declared"

---------- VAR BLOCK START ----------

constDef :: ConstDef -> PaskellState ()
constDef (name, vl) = do
    (constTable, varTable, funcTable) <- get
    put (Map.insert name (evalValueLiteral vl) constTable, varTable, funcTable)

varDef :: VarDef -> PaskellState ()
varDef v = do
    (constTable, varTable, funcTable) <- get
    let (s, t, me) = v
    mv <- case me of
        Just expr -> Just <$> evalExpr expr
        Nothing   -> return Nothing
    let (_, _, newVarTable) = foldl varListInsert (t, mv, varTable) s
    put (constTable, newVarTable, funcTable)
    -- TODO: Check if type and Value match

varListInsert
    :: (VarType, Maybe Value, VarTable)
    -> Text
    -> (VarType, Maybe Value, VarTable)
varListInsert (vt, mv, varTable) t = do
    let mvlookup = Map.lookup t varTable
    case mvlookup of
        Just vlookup -> error $ "Variable ->" ++ show t ++ "<- was redefined."
        Nothing      -> if valueMatch vt mv || isNothing mv
            then (vt, mv, Map.insert t (vt, mv) varTable)
            else
                error
                $  "Variable type ->"
                ++ show vt
                ++ "<- does not match assignment type ->"
                ++ show mv
                ++ "<-"

---------- VAR BLOCK START ----------

varGet :: Text -> PaskellState (VarType, Maybe Value)
varGet name = do
    (constTable, varTable, _) <- get
    pure $ varPureGet name (constTable, varTable)

maybeVarGet :: Text -> PaskellState (Maybe (VarType, Maybe Value))
maybeVarGet name = do
    (constTable, varTable, _) <- get
    pure $ maybeVarPureGet name (constTable, varTable)

varPureGet :: Text -> (ConstTable, VarTable) -> (VarType, Maybe Value)
varPureGet name (constTable, varTable) = do
    let x = maybeVarPureGet name (constTable, varTable)
    fromMaybe
        (  error
        $  "Constant or variable ->"
        ++ show name
        ++ "<- was never declared, or used a constant when a variable was expected"
        )
        x

maybeVarPureGet
    :: Text -> (ConstTable, VarTable) -> Maybe (VarType, Maybe Value)
maybeVarPureGet name (constTable, varTable) = do
    let c = Map.lookup name constTable
    let v = Map.lookup name varTable
    case (c, v) of
        (Just jc, Just jv) ->
            error
                $  "Both a constant and a variable share the name ->"
                ++ show name
                ++ "<-"
        (Nothing, Just jv) -> Just jv
        (Just jc, Nothing) -> Just (valueType jc, Just jc)
        (Nothing, Nothing) -> Nothing


valueExist :: Maybe a -> a
valueExist t = case t of
    Just v  -> v
    Nothing -> error $ "Expected value to not be Nothing"

doAssign :: (Text, Maybe Value) -> PaskellState ()
doAssign (t, mv) = do
    (constTable, varTable, funcTable) <- get
    put (constTable, doPureAssign (t, mv) (constTable, varTable), funcTable)

doPureAssign :: (Text, Maybe Value) -> (ConstTable, VarTable) -> VarTable
doPureAssign (t, mv) (constTable, varTable) = case mv of
    Just (VBool b) -> if valueMatch vt mv
        then Map.insert t (vt, mv) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be boolean type"
    Just (VInt n) -> if valueMatch vt mv
        then Map.insert t (vt, mv) varTable
        else if valueMatch vt (Just . VDouble $ fromIntegral n)
            then Map.insert t (vt, Just . VDouble $ fromIntegral n) varTable
            else
                error
                $  "Expected ->"
                ++ show t
                ++ "<- to be numeric type (int or real)"
    Just (VDouble d) -> if valueMatch vt mv
        then Map.insert t (vt, mv) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be double type"
    Just (VString s) -> if valueMatch vt mv
        then Map.insert t (vt, mv) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be string type"
    -- TODO: Properly handle enums
    Just (VEnum en) -> if valueMatch vt mv
        then Map.insert t (vt, mv) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be enum type"
    Nothing -> error $ "Variable ->" ++ show t ++ "<- cannot be set to Nothing"
    where (vt, _) = varPureGet t (Map.empty, varTable)

valueMatch :: VarType -> Maybe Value -> Bool
valueMatch vt mv = case mv of
    Just v -> case v of
        VBool b -> case vt of
            BoolType -> True
            _        -> False
        VInt n -> case vt of
            IntType -> True
            _       -> False
        VDouble d -> case vt of
            RealType -> True
            _        -> False
        VString s -> case vt of
            StringType -> True
            _          -> False
            -- TODO: Properly handle enums
        VEnum en -> case vt of
            EnumType _ -> True
            _          -> False
    Nothing -> False


valueType :: Value -> VarType
valueType val = case val of
    VBool   b  -> BoolType
    VInt    n  -> IntType
    VDouble d  -> RealType
    VString s  -> StringType
        -- TODO: Properly handle enums
    VEnum   en -> EnumType "taco"

---------- EXPR START ----------

evalValueLiteral :: ValueLiteral -> Value
evalValueLiteral (Int    i)         = VInt i
evalValueLiteral (Double d)         = VDouble d
evalValueLiteral BTrue              = VBool True
evalValueLiteral BFalse             = VBool False
evalValueLiteral (StringLiteral sl) = VString sl

execFunc :: Text -> [Expr] -> PaskellState (Maybe Value)
execFunc name callParam = do
    (_, _, funcTable) <- get
    let f = funcPureGet name funcTable
    case f of
        Function (FunctionDec name paramList (Just returnType)) blocks stat ->
            do
            -- TODO: Create a new scope here
            -- execute
                functionParamHandle callParam paramList
                (constTable, varTable, funcTable) <- get
                -- expecting return value in variable with function name.
                put
                    ( constTable
                    , Map.insert name (returnType, Nothing) varTable
                    , funcTable
                    )
                mapM_ execBlock blocks
                execStatement stat
                (_, mv) <- varGet name
                -- TODO: Clean up scope
                if valueMatch returnType mv
                    then return mv
                    else return $ error "Function returned mismatched type"
        Function (FunctionDec name paramList Nothing) blocks stat -> do
            -- TODO: Create a new scope here
            -- execute
            functionParamHandle callParam paramList
            mapM_ execBlock blocks
            execStatement stat
            -- TODO: Clean up scope
            return Nothing

-- This will validate paramlist and inject values into the state.
functionParamHandle :: [Expr] -> [FuncParam] -> PaskellState ()
functionParamHandle callParam funcParam = do
    evalCallParam <- mapM evalExpr callParam
    let funcParamList = expandFuncParam funcParam
    injectFunctionParams evalCallParam funcParamList

injectFunctionParams :: [Value] -> [(Text, VarType)] -> PaskellState ()
injectFunctionParams (v : vs) ((name, vt) : ps) = do
    (constTable, varTable, funcTable) <- get
    if valueMatch vt (Just v)
        then
            put (constTable, Map.insert name (vt, Just v) varTable, funcTable)
                >> injectFunctionParams vs ps
        else error "Function parameter type not matching"
injectFunctionParams [] [] = return ()
injectFunctionParams _  _  = error "Incorrect number of function parameters"

expandFuncParam :: [FuncParam] -> [(Text, VarType)]
expandFuncParam ((name, vt) : fps) =
    map ((\vt text -> (text, vt)) vt) name ++ expandFuncParam fps
expandFuncParam [] = []

evalExpr :: Expr -> PaskellState Value
evalExpr expr = case expr of
    VarCall name [] -> do
        mvg <- maybeVarGet name
        case mvg of
            Just (_, mv) -> case mv of
                Just v  -> return v
                Nothing -> valueExist <$> execFunc name []
            Nothing -> valueExist <$> execFunc name []
    VarCall name param ->
        snd <$> varGet name >>= \mv -> valueExist <$> execFunc name param
    VExpr vl -> return $ evalValueLiteral vl
    Neg   n  -> do
        n1e <- evalExpr n
        case n1e of
            VInt    ie -> return $ VInt (-ie)
            VDouble de -> return $ VDouble (-de)
            _ ->
                error
                    $  "Expected ->"
                    ++ show n1e
                    ++ "<- to be numeric type (int or real)"
    Sum s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 + i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 + d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 + fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 + d2)
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be numeric type (int or real)"
    Sub s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 - i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 - d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 - fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 - d2)
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be numeric type (int or real)"
    Mul s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 * i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 * d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 * fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 * d2)
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be numeric type (int or real)"
    Div s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 `quot` i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 / d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 / fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 / d2)
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be numeric type (int or real)"
    Mod s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt i1, VInt i2) -> return $ VInt (i1 `mod'` i2)
            (VInt i1, VDouble d2) ->
                return $ VDouble $ (fromIntegral i1 `mod'` d2)
            (VDouble d1, VInt i2) ->
                return $ VDouble $ (d1 `mod'` fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 `mod'` d2)
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be numeric type (int or real)"
    Eq s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt i1, VDouble d2) ->
                return $ VBool (abs (fromIntegral i1 - d2) < 0.00001)
            (VDouble d1, VInt i2) ->
                return $ VBool (abs (d1 - fromIntegral i2) < 0.00001)
            (VDouble d1, VDouble d2) ->
                return $ VBool (abs (d1 - d2) < 0.00001)
            _ -> return $ VBool $ s1e == s2e
    NotEq s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt i1, VDouble d2) ->
                return $ VBool (abs (fromIntegral i1 - d2) >= 0.00001)
            (VDouble d1, VInt i2) ->
                return $ VBool (abs (d1 - fromIntegral i2) >= 0.00001)
            (VDouble d1, VDouble d2) ->
                return $ VBool (abs (d1 - d2) >= 0.00001)
            _ -> return $ VBool $ s1e /= s2e
    GreaterThan s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        return $ VBool $ s1e > s2e
    LessThan s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        return $ VBool $ s1e < s2e
    GreaterThanEq s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt i1, VDouble d2) ->
                return $ VBool
                    (s1e > s2e || (abs (fromIntegral i1 - d2) < 0.00001))
            (VDouble d1, VInt i2) ->
                return $ VBool
                    (s1e > s2e || (abs (d1 - fromIntegral i2) < 0.00001))
            (VDouble d1, VDouble d2) ->
                return $ VBool (s1e > s2e || (abs (d1 - d2) < 0.00001))
            _ -> return $ VBool $ s1e >= s2e
    LessThanEq s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VInt i1, VDouble d2) ->
                return $ VBool
                    (s1e < s2e || (abs (fromIntegral i1 - d2) < 0.00001))
            (VDouble d1, VInt i2) ->
                return $ VBool
                    (s1e < s2e || (abs (d1 - fromIntegral i2) < 0.00001))
            (VDouble d1, VDouble d2) ->
                return $ VBool (s1e < s2e || (abs (d1 - d2) < 0.00001))
            _ -> return $ VBool $ s1e <= s2e
    Not s1 -> do
        s1e <- evalExpr s1
        case s1e of
            VBool b -> return $ VBool $ not b
            VInt i -> return $ VInt $ complement i
            _ -> error $ "Expected ->" ++ show s1e ++ "<- to be boolean or int"
    Or s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
            (VInt  i1, VInt i2 ) -> return $ VInt $ i1 .|. i2
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be boolean or int"
    -- TODO: Should not be the same as or
    OrElse s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VBool b1, VBool b2) -> return $ VBool $ b1 || b2
            (VInt  i1, VInt i2 ) -> return $ VInt $ i1 .|. i2
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be boolean or int"
    And s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
            (VInt  i1, VInt i2 ) -> return $ VInt $ i1 .&. i2
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be boolean or int"
    -- TODO: Should not be the same as and
    AndThen s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VBool b1, VBool b2) -> return $ VBool $ b1 && b2
            (VInt  i1, VInt i2 ) -> return $ VInt $ i1 .&. i2
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be boolean or int"
    Xor s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VBool b1, VBool b2) -> return $ VBool $ booleanXor b1 b2
            (VInt  i1, VInt i2 ) -> return $ VInt $ xor i1 i2
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be boolean or int"
    -- TODO: Implement shift left and shift right
    StringConcat s1 s2 -> do
        s1e <- evalExpr s1
        s2e <- evalExpr s2
        case (s1e, s2e) of
            (VString sl1, VString sl2) ->
                return $ VString $ pack $ unpack sl1 ++ unpack sl2
            _ ->
                error
                    $  "Expected ->"
                    ++ show s1e
                    ++ "<- and ->"
                    ++ show s2e
                    ++ "<- to be string"
    _ -> error $ "Invalid expression ->" ++ show expr ++ "<-"

booleanXor :: Bool -> Bool -> Bool
booleanXor True  p = not p
booleanXor False p = p
---------- EXPR END ----------
