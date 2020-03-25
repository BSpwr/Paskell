module TypeAST where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           TypeValue

data AST
    = Node String AST AST
    | ProgBlock Statement
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
    = StatementBlock [Statement]
    | Assign (Text, Expr)
    | Writeln [Expr]
    | Readln [Text]
    | StatementIf Expr Statement (Maybe Statement)
    | StatementCase Expr
    deriving (Show)

data CaseLine = Expr Statement


data ValueLiteral =
    Int Int
    | Double Double
    | BFalse
    | BTrue
    | StringLiteral Text
    deriving (Eq, Ord, Show)

data Expr
    = Var Text
    | VExpr ValueLiteral
    | Neg Expr
    | Sum Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Mod Expr Expr
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
    | StringConcat Expr Expr
    deriving (Eq, Ord, Show)

-- class ShowBase v where

