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
    | RawVEnum Text
    | VEnum Text Text -- Name, EnumName
    deriving (Eq, Ord, Show)
