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
                                                )

import           TypeAST
import           TypeValue

type PaskellState = StateT VarTable IO

type VarTable = Map Text (VarType, Maybe Value)

-- clearState :: PaskellState ()
-- clearState = put Map.empty

interpreterRun :: AST -> IO ()
interpreterRun ast = evalStateT (interpreterStart ast) Map.empty

interpreterStart :: AST -> PaskellState ()
interpreterStart (Node "Root" a b) = do
    -- x <- liftIO readBool
    -- liftIO $ print x
    interpret a
    interpret b
    -- liftIO $ putStrLn "root"
    -- varTable <- get
    -- liftIO $ print varTable
    -- liftIO $ putStrLn "\n"
    -- liftIO $ print (Node "Root" a b)

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
interpret (VarBlock  vb) = mapM_ varDef vb
interpret (ProgBlock sb) = execStatement sb

execStatement :: Statement -> PaskellState ()
execStatement (Assign (t, expr)) = do
    v <- evalExpr expr
    doAssign (t, v)

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
        (VInt i, VInt f) ->
            doAssign (t, vi) >> execForLoop stat (t, i, loopDir, f)
        _ -> error "For loop variable must be integer type"

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

execForLoop
    :: Statement -> (Text, Int, ForLoopDirection, Int) -> PaskellState ()
execForLoop stat (t, c, loopDir, f) = do
    let (status, next) = case loopDir of
            To     -> (c <= f, c + 1)
            DownTo -> (c >= f, c - 1)
    when status
        $  doAssign (t, VInt c)
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
    varTable <- get
    (vt, mv) <- varGet t
    v        <- liftIO $ readValue vt
    put (Map.insert t (vt, Just v) varTable)

printValue :: Value -> PaskellState ()
printValue v = liftIO $ putStr $ showValue v

showValue :: Value -> String
showValue (VBool   b) = show b
showValue (VInt    i) = show i
showValue (VDouble d) = show d
showValue (VString s) = unpack s
showValue (VEnum   e) = unpack e

---------- VAR BLOCK START ----------

varDef :: VarDef -> PaskellState ()
varDef v = modify $ varPureDef v

varPureDef :: VarDef -> VarTable -> VarTable
varPureDef v state = newVarTable
  where
    (s, t, me         ) = v
    (_, _, newVarTable) = foldl varListInsert (t, mv, state) s
    mv                  = case me of
        Just expr -> Just $ evalPureExpr expr state
        Nothing   -> Nothing

varListInsert
    :: (VarType, Maybe Value, VarTable)
    -> Text
    -> (VarType, Maybe Value, VarTable)
varListInsert (vt, mv, varTable) t = if valueMatch vt mv || isNothing mv
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
varGet t = gets $ varPureGet t

varPureGet :: Text -> VarTable -> (VarType, Maybe Value)
varPureGet t varTable = do
    let v = Map.lookup t varTable
    case v of
        Just v  -> v
        Nothing -> error $ "Variable ->" ++ show t ++ "<- was never declared"

valueExist :: Maybe Value -> Value
valueExist t = case t of
    Just v  -> v
    Nothing -> error $ "Variable ->" ++ show t ++ "<- was never assigned"

doAssign :: (Text, Value) -> PaskellState ()
doAssign (t, v) = modify $ doPureAssign (t, v)

doPureAssign :: (Text, Value) -> VarTable -> VarTable
doPureAssign (t, v) varTable = case v of
    VBool b -> if valueMatch vt (Just v)
        then Map.insert t (vt, Just v) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be boolean type"
    VInt n -> if valueMatch vt (Just v)
        then Map.insert t (vt, Just v) varTable
        else if valueMatch vt (Just . VDouble $ fromIntegral n)
            then Map.insert t (vt, Just . VDouble $ fromIntegral n) varTable
            else
                error
                $  "Expected ->"
                ++ show t
                ++ "<- to be numeric type (int or real)"
    VDouble d -> if valueMatch vt (Just v)
        then Map.insert t (vt, Just v) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be double type"
    VString s -> if valueMatch vt (Just v)
        then Map.insert t (vt, Just v) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be string type"
    -- TODO: Properly handle enums
    VEnum en -> if valueMatch vt (Just v)
        then Map.insert t (vt, Just v) varTable
        else error $ "Expected ->" ++ show t ++ "<- to be enum type"
    where (vt, _) = varPureGet t varTable

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

---------- EXPR START ----------

evalValueLiteral :: ValueLiteral -> Value
evalValueLiteral (Int    i)         = VInt i
evalValueLiteral (Double d)         = VDouble d
evalValueLiteral BTrue              = VBool True
evalValueLiteral BFalse             = VBool False
evalValueLiteral (StringLiteral sl) = VString sl

evalExpr :: Expr -> PaskellState Value
evalExpr expr = gets $ evalPureExpr expr

evalPureExpr :: Expr -> VarTable -> Value
evalPureExpr expr state = case expr of
    Var   t  -> let (_, v) = varPureGet t state in valueExist v

    VExpr vl -> evalValueLiteral vl

    Neg   n  -> case n1e of
        VInt    ie -> VInt (-ie)
        VDouble de -> VDouble (-de)
        _ ->
            error
                $  "Expected ->"
                ++ show n1e
                ++ "<- to be numeric type (int or real)"
        where n1e = evalPureExpr n state

    Sum s1 s2 -> case (s1e, s2e) of
        (VInt    i1, VInt i2   ) -> VInt (i1 + i2)
        (VInt    i1, VDouble d2) -> VDouble (fromIntegral i1 + d2)
        (VDouble d1, VInt i2   ) -> VDouble (d1 + fromIntegral i2)
        (VDouble d1, VDouble d2) -> VDouble (d1 + d2)
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be numeric type (int or real)"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Sub s1 s2 -> case (s1e, s2e) of
        (VInt    i1, VInt i2   ) -> VInt (i1 - i2)
        (VInt    i1, VDouble d2) -> VDouble (fromIntegral i1 - d2)
        (VDouble d1, VInt i2   ) -> VDouble (d1 - fromIntegral i2)
        (VDouble d1, VDouble d2) -> VDouble (d1 - d2)
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be numeric type (int or real)"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Mul s1 s2 -> case (s1e, s2e) of
        (VInt    i1, VInt i2   ) -> VInt (i1 * i2)
        (VInt    i1, VDouble d2) -> VDouble (fromIntegral i1 * d2)
        (VDouble d1, VInt i2   ) -> VDouble (d1 * fromIntegral i2)
        (VDouble d1, VDouble d2) -> VDouble (d1 * d2)
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be numeric type (int or real)"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Div s1 s2 -> case (s1e, s2e) of
        (VInt    i1, VInt i2   ) -> VInt (i1 `quot` i2)
        (VInt    i1, VDouble d2) -> VDouble (fromIntegral i1 / d2)
        (VDouble d1, VInt i2   ) -> VDouble (d1 / fromIntegral i2)
        (VDouble d1, VDouble d2) -> VDouble (d1 / d2)
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be numeric type (int or real)"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Mod s1 s2 -> case (s1e, s2e) of
        (VInt    i1, VInt i2   ) -> VInt (i1 `mod'` i2)
        (VInt    i1, VDouble d2) -> VDouble $ (fromIntegral i1 `mod'` d2)
        (VDouble d1, VInt i2   ) -> VDouble $ (d1 `mod'` fromIntegral i2)
        (VDouble d1, VDouble d2) -> VDouble (d1 `mod'` d2)
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be numeric type (int or real)"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Eq s1 s2 -> case (s1e, s2e) of
        (VInt    i1, VDouble d2) -> VBool (abs (fromIntegral i1 - d2) < 0.00001)
        (VDouble d1, VInt i2   ) -> VBool (abs (d1 - fromIntegral i2) < 0.00001)
        (VDouble d1, VDouble d2) -> VBool (abs (d1 - d2) < 0.00001)
        _                        -> VBool $ s1e == s2e
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    NotEq s1 s2 -> case (s1e, s2e) of
        (VInt i1, VDouble d2) -> VBool (abs (fromIntegral i1 - d2) >= 0.00001)
        (VDouble d1, VInt i2) -> VBool (abs (d1 - fromIntegral i2) >= 0.00001)
        (VDouble d1, VDouble d2) -> VBool (abs (d1 - d2) >= 0.00001)
        _ -> VBool $ s1e /= s2e
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    GreaterThan s1 s2 -> VBool $ s1e > s2e
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    LessThan s1 s2 -> VBool $ s1e < s2e
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    GreaterThanEq s1 s2 -> case (s1e, s2e) of
        (VInt i1, VDouble d2) ->
            VBool (s1e > s2e || (abs (fromIntegral i1 - d2) < 0.00001))
        (VDouble d1, VInt i2) ->
            VBool (s1e > s2e || (abs (d1 - fromIntegral i2) < 0.00001))
        (VDouble d1, VDouble d2) ->
            VBool (s1e > s2e || (abs (d1 - d2) < 0.00001))
        _ -> VBool $ s1e >= s2e
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    LessThanEq s1 s2 -> case (s1e, s2e) of
        (VInt i1, VDouble d2) ->
            VBool (s1e < s2e || (abs (fromIntegral i1 - d2) < 0.00001))
        (VDouble d1, VInt i2) ->
            VBool (s1e < s2e || (abs (d1 - fromIntegral i2) < 0.00001))
        (VDouble d1, VDouble d2) ->
            VBool (s1e < s2e || (abs (d1 - d2) < 0.00001))
        _ -> VBool $ s1e <= s2e
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Not s1 -> case s1e of
        VBool b -> VBool $ not b
        VInt i -> VInt $ complement i
        _ -> error $ "Expected ->" ++ show s1e ++ "<- to be boolean or int"
        where s1e = evalPureExpr s1 state

    Or s1 s2 -> case (s1e, s2e) of
        (VBool b1, VBool b2) -> VBool $ b1 || b2
        (VInt  i1, VInt i2 ) -> VInt $ i1 .|. i2
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be boolean or int"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    -- TODO: Should not be the same as or
    OrElse s1 s2 -> case (s1e, s2e) of
        (VBool b1, VBool b2) -> VBool $ b1 || b2
        (VInt  i1, VInt i2 ) -> VInt $ i1 .|. i2
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be boolean or int"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    And s1 s2 -> case (s1e, s2e) of
        (VBool b1, VBool b2) -> VBool $ b1 && b2
        (VInt  i1, VInt i2 ) -> VInt $ i1 .&. i2
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be boolean or int"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    -- TODO: Should not be the same as and
    AndThen s1 s2 -> case (s1e, s2e) of
        (VBool b1, VBool b2) -> VBool $ b1 && b2
        (VInt  i1, VInt i2 ) -> VInt $ i1 .&. i2
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be boolean or int"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    Xor s1 s2 -> case (s1e, s2e) of
        (VBool b1, VBool b2) -> VBool $ booleanXor b1 b2
        (VInt  i1, VInt i2 ) -> VInt $ xor i1 i2
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be boolean or int"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    -- TODO: Implement shift left and shift right
    StringConcat s1 s2 -> case (s1e, s2e) of
        (VString sl1, VString sl2) -> VString $ pack $ unpack sl1 ++ unpack sl2
        _ ->
            error
                $  "Expected ->"
                ++ show s1e
                ++ "<- and ->"
                ++ show s2e
                ++ "<- to be string"
      where
        s1e = evalPureExpr s1 state
        s2e = evalPureExpr s2 state

    _ -> error $ "Invalid expression ->" ++ show expr ++ "<-"

booleanXor :: Bool -> Bool -> Bool
booleanXor True  p = not p
booleanXor False p = p
---------- EXPR END ----------
