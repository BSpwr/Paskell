module Interpreter where

import           Control.Monad.State
import           Control.Monad.IO.Class
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Stack                     ( Stack )
import qualified Data.Stack                    as Stack

import           Data.Maybe                     ( maybe
                                                , Maybe(..)
                                                )

import           TypeAST
import           TypeValue

type PaskellState = StateT VarTable IO ()

type VarTable = Map String Value

interpreterRun :: AST -> IO ()
interpreterRun ast = evalStateT (interpreterStart ast) Map.empty

interpreterStart :: AST -> PaskellState
interpreterStart (Node "Root" a b) = do
    liftIO $ putStrLn "root"
    liftIO $ print (Node "Root" a b)

-- iVarBlock :: AST -> PaskellState
-- iVarBlock VarBlock = 


-- iVarDef :: VarDef -> PaskellState
-- iVarDef v = do
--     varTable <- get
--     case t of
--         BoolType ->
--             maybe
--         IntType ->
--         RealType ->
--         StringType ->
--         EnumType s ->
--     where
--         (s,t,g) = v
