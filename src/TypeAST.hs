module TypeAST where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           TypeValue

data AST
    = Node String AST AST
    | ProgBlock [Statement]
    | VarBlock [VarDef]
    | Nil
    deriving (Show)

type VarDef = ([Text], VarType, Maybe Expr)
data VarType
     = BoolType
     | IntType
     | RealType
     | StringType
     | EnumType String
     deriving (Show, Eq)

data Statement
    = Assign (Text, Expr)
    | Writeln [Expr]
    | Readln [Text]
    deriving (Show)
data Expr
    = Var Text
    | Int Int
    | Double Double
    | Neg Expr
    | Sum Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
    | BFalse
    | BTrue
    | Eq Expr Expr
    | NotEq Expr Expr
    | GreaterThan Expr Expr
    | LessThan Expr Expr
    | GreaterThanEq Expr Expr
    | LessThanEq Expr Expr
    | Not Expr
    | Or Expr Expr
    | OrElse Expr Expr
    | And Expr Expr
    | AndThen Expr Expr
    | Xor Expr Expr
    | ShiftLeft Expr Expr
    | ShiftRight Expr Expr
    | StringLiteral Text
    | StringConcat Expr Expr
    deriving (Eq, Ord, Show)

-- class ShowBase v where

