module TypeValue where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )

data Value
    = VBool Bool
    | VInt Int
    | VDouble Double
    | VString Text
    | VEnum Text
    deriving (Eq, Ord, Show)
