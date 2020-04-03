module TypeAST where

import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map

import           Data.Stack                     ( Stack )
import qualified Data.Stack                    as Stack

data AST
    = Node String AST AST
    | Block [BlockDef]
    | ProgBlock Statement
    | Nil
    deriving (Show)

data BlockDef = VarBlock [VarDef] | FuncBlock [Function] | ConstBlock [ConstDef] | TypeBlock [TypeDef] deriving (Show)

type VarDef = ([Text], VarType, Maybe Expr)
-- TypeDef only handles enum custom types
type TypeDef = (Text, [Text])
type ConstDef = (Text, ValueLiteral)

data VarType
     = BoolType
     | IntType
     | RealType
     | StringType
     | EnumType Text
     deriving (Show, Eq)

data ForLoopDirection = To | DownTo deriving (Show, Enum)

type FuncParam = ([Text], VarType)
type FuncReturnType = Maybe VarType
data FunctionDec = FunctionDec Text [FuncParam] FuncReturnType deriving (Show)
data Function = Function FunctionDec [BlockDef] Statement deriving (Show)

data Statement
    = StatementBlock [Statement]
    | Assign (Text, Expr)
    | Writeln [Expr]
    | Readln [Text]
    | ProcCall Text [Expr]
    | StatementIf Expr Statement (Maybe Statement)
    | StatementCase Expr [CaseLine] (Maybe Statement)
    | StatementWhile Expr Statement
    | StatementFor (Text, Expr) ForLoopDirection Expr Statement
    | StatementRepeatUntil [Statement] Expr
    | StatementBreak
    | StatementContinue
    deriving (Show)

data CaseLine
    = CaseLine [ValueLiteral] Statement
    deriving (Show)

data ValueLiteral =
    Int Int
    | Double Double
    | BFalse
    | BTrue
    | StringLiteral Text
    deriving (Eq, Ord, Show)

data Expr
    =  Text
    | VarCall Text [Expr]
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
    deriving (Eq, Show)

type InterpreterState = (InterpreterContext, ContextStack)

type ContextStack = Stack InterpreterContext
type InterpreterContext = (ConstTable, VarTable, TypeTable, FuncTable)

type ConstTable = Map Text Value
type VarTable = Map Text (VarType, Maybe Value)
type TypeTable = Map Text [Text]
type FuncTable = Map Text Function

data Value
    = VBool Bool
    | VInt Int
    | VDouble Double
    | VString Text
    | RawVEnum Text
    | VEnum Text Text -- Name, EnumName
    deriving (Eq, Ord, Show)

