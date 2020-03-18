module TypeValue where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )

data Value
    = VBool Bool
    | VNumeric Numeric
    | VString Text
    | VEnum Text
    deriving (Eq, Show)

data Numeric = VInt Int | VDouble Double  deriving (Eq, Ord, Show)
