module TypeAST where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           TypeValue

data AST
    = Node String AST AST
    | ProgBlock [Impl]
    | VarBlock [VarDef]
    | Nil
    deriving (Show)

type VarDef = ([Text], VarType, Maybe GenExpr)
data VarType = BoolType | IntType | RealType | StringType | EnumType String deriving (Show, Eq)

data Impl
    = Assign (Text, GenExpr)
    | Writeln [GenExpr]
    | Readln [Text]
    deriving (Show)

data GenExpr = NumExpr NumExpr | BoolExpr BoolExpr | StringExpr StringExpr deriving (Show)

data StringExpr
    = SVar Text
    | StringE Text
    | Concat StringExpr StringExpr
    deriving (Eq, Ord, Show)

data NumExpr
    = NVar Text
    | Int Int
    | Double Double
    | Neg NumExpr
    | Sum NumExpr NumExpr
    | Sub NumExpr NumExpr
    | Mul NumExpr NumExpr
    | Div NumExpr NumExpr
    deriving (Eq, Ord, Show)

data BoolExpr
    = BVar Text
    | Not BoolExpr
    | Or BoolExpr BoolExpr
    | And BoolExpr BoolExpr
    | Xor BoolExpr BoolExpr
    | BFalse
    | BTrue
    deriving (Eq, Ord, Show)
