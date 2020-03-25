module Interpreter where

import           Control.Monad.State
import           Control.Monad
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
interpret (VarBlock  vb) = mapM_ iVarDef vb
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
            then Control.Monad.void $ execStatement s1
            else case s2 of
                Just s  -> Control.Monad.void $ execStatement s
                Nothing -> return ()
        _ ->
            error
                $ "Expecting boolean type expression for if statement, instead received ->"
                ++ show ev
                ++ "<-"

execStatement (StatementBlock sb) = mapM_ execStatement sb

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
iVarDef :: VarDef -> PaskellState ()
iVarDef v = do
    let (s, t, me) = v
    let ps_e = case me of
            Just expr -> Just <$> evalExpr expr
            Nothing   -> return Nothing
    e <- ps_e
    -- TODO: Check if type and Value match
    foldM_ varListInsert (t, e) s

varListInsert
    :: (VarType, Maybe Value) -> Text -> PaskellState (VarType, Maybe Value)
varListInsert (vt, mv) t = do
    varTable <- get
    if valueMatch vt mv || isNothing mv
        then put (Map.insert t (vt, mv) varTable)
        else
            liftIO
            $  error
            $  "Variable type ->"
            ++ show vt
            ++ "<- does not match assignment type ->"
            ++ show mv
            ++ "<-"
    return (vt, mv)
---------- VAR BLOCK START ----------

varGet :: Text -> PaskellState (VarType, Maybe Value)
varGet t = do
    varTable <- get
    let v = Map.lookup t varTable
    case v of
        Just v  -> return v
        Nothing -> error $ "Variable ->" ++ show t ++ "<- was never declared"

valueExist :: Maybe Value -> Value
valueExist t = case t of
    Just v  -> v
    Nothing -> error $ "Variable ->" ++ show t ++ "<- was never assigned"

doAssign :: (Text, Value) -> PaskellState ()
doAssign (t, v) = do
    varTable <- get
    (vt, _)  <- varGet t
    case v of
        VBool b -> if valueMatch vt (Just v)
            then put (Map.insert t (vt, Just v) varTable)
            else error $ "Expected ->" ++ show t ++ "<- to be boolean type"
        VInt n -> if valueMatch vt (Just v)
            then put (Map.insert t (vt, Just v) varTable)
            else if valueMatch vt (Just . VDouble $ fromIntegral n)
                then
                    put
                        (Map.insert t
                                    (vt, Just . VDouble $ fromIntegral n)
                                    varTable
                        )
                else
                    error
                    $  "Expected ->"
                    ++ show t
                    ++ "<- to be numeric type (int or real)"
        VDouble d -> if valueMatch vt (Just v)
            then put (Map.insert t (vt, Just v) varTable)
            else error $ "Expected ->" ++ show t ++ "<- to be double type"
        VString t -> if valueMatch vt (Just v)
            then put (Map.insert t (vt, Just v) varTable)
            else error $ "Expected ->" ++ show t ++ "<- to be string type"
        -- TODO: Properly handle enums
        VEnum t -> if valueMatch vt (Just v)
            then put (Map.insert t (vt, Just v) varTable)
            else error $ "Expected ->" ++ show t ++ "<- to be enum type"

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
evalExpr :: Expr -> PaskellState Value
evalExpr expr = case expr of
    Var t -> do
        (_, v) <- varGet t
        return $ valueExist v
    VExpr (Int    i) -> return $ VInt i
    VExpr (Double d) -> return $ VDouble d
    Neg   n          -> do
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
    VExpr BTrue  -> return $ VBool True
    VExpr BFalse -> return $ VBool False
    Eq s1 s2     -> do
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
    VExpr (StringLiteral sl) -> return $ VString sl
    StringConcat s1 s2       -> do
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
