module Interpreter where

import           Control.Monad.State
import           Control.Monad.IO.Class
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Stack                     ( Stack )
import qualified Data.Stack                    as Stack
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , toLower
                                                )
import           Data.Maybe                     ( maybe
                                                , Maybe(..)
                                                )

import           TypeAST
import           TypeValue

type PaskellState = StateT VarTable IO

type VarTable = Map Text (VarType, Maybe Value)

interpreterRun :: AST -> IO ()
interpreterRun ast = evalStateT (interpreterStart ast) Map.empty

interpreterStart :: AST -> PaskellState ()
interpreterStart (Node "Root" a b) = do
    interpret a
    interpret b
    -- liftIO $ putStrLn "root"
    -- varTable <- get
    -- liftIO $ print varTable
    -- liftIO $ putStrLn "\n"
    -- liftIO $ print (Node "Root" a b)

interpret :: AST -> PaskellState ()
interpret (VarBlock  vb   ) = mapM_ iVarDef vb
interpret (ProgBlock impls) = mapM_ implementation impls

implementation :: Impl -> PaskellState ()
implementation (Assign (t, ge)) = do
    v <- evalGenExpr ge
    doAssign (t, v)

implementation (Writeln genExprs) = do
    vs <- mapM evalGenExpr genExprs
    mapM_ printValue vs

-- implementation (Readln varNames) = do
--     vs <- mapM evalGenExpr genExprs
--     mapM_ printValue vs

printValue :: Value -> PaskellState ()
printValue v = liftIO $ print v

---------- VAR BLOCK START ----------
iVarDef :: VarDef -> PaskellState ()
iVarDef v = do
    varTable <- get
    let (s, t, g) = v
    let ps_e = case g of
            Just ge -> Just <$> evalGenExpr ge
            Nothing -> return Nothing
    e <- ps_e
    -- TODO: Check if type and Value match
    foldM_ varListInsert (t, e) s

varListInsert
    :: (VarType, Maybe Value) -> Text -> PaskellState (VarType, Maybe Value)
varListInsert (vt, mv) t = do
    varTable <- get
    put (Map.insert t (vt, mv) varTable)
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
        VBool b -> case valueMatch vt (Just v) of
            True  -> put (Map.insert t (vt, Just v) varTable)
            False -> error $ "Expected ->" ++ show t ++ "<- to be boolean type"
        VNumeric (VInt n) -> case valueMatch vt (Just v) of
            True -> put (Map.insert t (vt, Just v) varTable)
            False ->
                case
                        valueMatch
                            vt
                            (Just . VNumeric . VDouble $ fromIntegral n)
                    of
                        True ->
                            put
                                (Map.insert
                                    t
                                    ( vt
                                    , Just . VNumeric . VDouble $ fromIntegral n
                                    )
                                    varTable
                                )
                        False ->
                            error
                                $  "Expected ->"
                                ++ show t
                                ++ "<- to be numeric type (int or real)"
        VNumeric (VDouble d) -> case valueMatch vt (Just v) of
            True  -> put (Map.insert t (vt, Just v) varTable)
            False -> error $ "Expected ->" ++ show t ++ "<- to be double type"
        VString t -> case valueMatch vt (Just v) of
            True  -> put (Map.insert t (vt, Just v) varTable)
            False -> error $ "Expected ->" ++ show t ++ "<- to be string type"
        -- TODO: Properly handle enums
        VEnum t -> case valueMatch vt (Just v) of
            True  -> put (Map.insert t (vt, Just v) varTable)
            False -> error $ "Expected ->" ++ show t ++ "<- to be enum type"

valueMatch :: VarType -> Maybe Value -> Bool
valueMatch vt mv = do
    case mv of
        Just v -> case v of
            VBool b -> case vt of
                BoolType -> True
                _        -> False
            VNumeric n -> case vt of
                IntType -> case n of
                    VInt i -> True
                    _      -> False
                RealType -> case n of
                    VDouble d -> True
                    _         -> False
            VString s -> case vt of
                StringType -> True
                _          -> False
                -- TODO: Properly handle enums
            VEnum en -> case vt of
                EnumType _ -> True
                _          -> False
        Nothing -> False

---------- EXPR START ----------
-- Because of the ordering of GenExpr in parser
-- lone variable names will be of type StringExpr, so handle accordingly
evalGenExpr :: GenExpr -> PaskellState Value
evalGenExpr (StringExpr se) = case se of
    SVar t -> do
        (vt, v) <- varGet t
        return $ valueExist v
    se -> VString <$> evalStringExpr se

evalGenExpr (BoolExpr be) = VBool <$> evalBoolExpr be

evalGenExpr (NumExpr  ne) = VNumeric <$> evalNumExpr ne

evalStringExpr :: StringExpr -> PaskellState Text
evalStringExpr se = case se of
    SVar t -> do
        (_, v) <- varGet t
        let ve = valueExist v
        case ve of
            VString sl -> return sl
            _ -> error $ "Expected ->" ++ show t ++ "<- to be string type"
    StringE sl   -> return sl
    Concat s1 s2 -> do
        s1e <- evalStringExpr s1
        s2e <- evalStringExpr s2
        return $ pack $ unpack s1e ++ unpack s2e

evalBoolExpr :: BoolExpr -> PaskellState Bool
evalBoolExpr be = case be of
    BTrue  -> return True
    BFalse -> return False
    BVar t -> do
        (_, v) <- varGet t
        let ve = valueExist v
        case ve of
            VBool sl -> return sl
            _ -> error $ "Expected ->" ++ show t ++ "<- to be boolean type"
    Not b    -> not <$> evalBoolExpr b
    Or b1 b2 -> do
        b1e <- evalBoolExpr b1
        b2e <- evalBoolExpr b2
        return (b1e || b2e)
    And b1 b2 -> do
        b1e <- evalBoolExpr b1
        b2e <- evalBoolExpr b2
        return (b1e && b2e)
    Xor b1 b2 -> do
        b1e <- evalBoolExpr b1
        b2e <- evalBoolExpr b2
        return (xor b1e b2e)

xor :: Bool -> Bool -> Bool
xor True  p = not p
xor False p = p

evalNumExpr :: NumExpr -> PaskellState Numeric
evalNumExpr ne = case ne of
    NVar t -> do
        (_, v) <- varGet t
        let ve = valueExist v
        case ve of
            VNumeric sl -> return sl
            _ ->
                error
                    $  "Expected ->"
                    ++ show t
                    ++ "<- to be numeric type (int or real)"
    Int    i -> return $ VInt i
    Double d -> return $ VDouble d
    Neg    n -> do
        n1e <- evalNumExpr n
        case n1e of
            VInt    ie -> return $ VInt (-ie)
            VDouble de -> return $ VDouble (-de)
    Sum s1 s2 -> do
        s1e <- evalNumExpr s1
        s2e <- evalNumExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 + i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 + d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 + fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 + d2)
    Sub s1 s2 -> do
        s1e <- evalNumExpr s1
        s2e <- evalNumExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 - i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 - d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 - fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 - d2)
    Mul s1 s2 -> do
        s1e <- evalNumExpr s1
        s2e <- evalNumExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 * i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 * d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 * fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 * d2)
    Div s1 s2 -> do
        s1e <- evalNumExpr s1
        s2e <- evalNumExpr s2
        case (s1e, s2e) of
            (VInt    i1, VInt i2   ) -> return $ VInt (i1 `quot` i2)
            (VInt    i1, VDouble d2) -> return $ VDouble (fromIntegral i1 / d2)
            (VDouble d1, VInt i2   ) -> return $ VDouble (d1 / fromIntegral i2)
            (VDouble d1, VDouble d2) -> return $ VDouble (d1 / d2)

getVar :: Text -> PaskellState Value
getVar t = do
    varTable <- get
    return $ VBool False
---------- EXPR END ----------
