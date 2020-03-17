module Interpreter where

import           Control.Monad.State
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Stack                     ( Stack )
import qualified Data.Stack                    as Stack

import           TypeValue

type PaskellRunner = StateT (VarTable) IO ()

type VarTable = Map String Value



