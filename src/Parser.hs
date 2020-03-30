{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Applicative     hiding ( some
                                                , many
                                                )
import           Control.Monad
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , toLower
                                                )
import           Data.Maybe                     ( maybe
                                                , Maybe(..)
                                                , fromMaybe
                                                )
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           System.Environment
import           Control.Monad.Combinators.Expr
import qualified Data.Text                     as T
import qualified Text.Megaparsec.Char.Lexer    as L

import           TypeAST

type Parser = Parsec Void Text

pRun :: Parser AST
pRun = pStart <* eof

pStart :: Parser AST
pStart = do
    pProgramHeader
    a1 <- optionalList pBlocks
    semi
    a2 <- pStatementBlock
    dot
    return $ Node "Root" (Block a1) (ProgBlock a2)

optionalList :: Alternative f => f [a] -> f [a]
optionalList f = fromMaybe [] <$> optional f

pProgramHeader :: Parser Text
pProgramHeader = try (pRW_program >> identifier) <|> pRW_program

pBlocks :: Parser [BlockDef]
pBlocks = try a <|> b  where
    a = do
        a1 <- pBlock
        semi
        a2 <- pBlocks
        return (a1 : a2)
    b = do
        b1 <- pBlock
        return [b1]

pBlock :: Parser BlockDef
pBlock = try (VarBlock <$> pVarBlocks) <|> (FuncBlock <$> pFuncDefBlock)

---------- VAR BLOCK WITH ASSIGNMENT START ----------

pVarBlocks :: Parser [VarDef]
pVarBlocks = try a <|> b  where
    a = do
        a1 <- pRW_var >> pVarDefs
        semi
        a2 <- pRW_var >> pVarDefs
        return (a1 ++ a2)
    b = pRW_var >> pVarDefs

pVarDefs :: Parser [VarDef]
pVarDefs = try a <|> b  where
    a = do
        a1 <- pVarDef
        semi
        a2 <- pVarDefs
        return (a1 : a2)
    b = do
        b1 <- pVarDef
        return [b1]

pVarDef :: Parser VarDef
pVarDef = try a <|> b  where
    a = do
        a1 <- pVarList
        col
        a2 <- pVarType
        equ
        a3 <- pExpr
        return (a1, a2, Just a3)
    b = do
        b1 <- pVarList
        col
        b2 <- pVarType
        return (b1, b2, Nothing)

pVarList :: Parser [Text]
pVarList = try a <|> b  where
    a = do
        a1 <- identifier
        com
        a2 <- pVarList
        return (a1 : a2)
    b = do
        b1 <- identifier
        return [b1]

pVarType :: Parser VarType
pVarType =
    (pRW_boolean >> return BoolType)
        <|> (pRW_integer >> return IntType)
        <|> (pRW_real >> return RealType)
        <|> (pRW_string >> return StringType)
        <|> (EnumType . unpack <$> identifier)
---------- VAL BLOCK WITH ASSIGNMENT END ----------

pFuncDefBlock :: Parser [Function]
pFuncDefBlock = try a <|> b  where
    a = do
        a1 <- pFunction
        semi
        a2 <- pFuncDefBlock
        return $ a1 : a2
    b = do
        b1 <- pFunction
        return [b1]

pFunction :: Parser Function
pFunction = do
    funcDef <- try pFunctionDec <|> pProcedureDec
    semi
    blocks <- optionalList pBlocks
    optional semi
    Function funcDef blocks <$> pStatementBlock

pFunctionDec :: Parser FunctionDec
pFunctionDec = do
    name  <- pRW_function >> identifier
    param <- optionalList $ parens pFunctionParam
    col
    FunctionDec name param . Just <$> pVarType

pProcedureDec :: Parser FunctionDec
pProcedureDec = do
    name  <- pRW_procedure >> identifier
    param <- optionalList $ parens pFunctionParam
    return $ FunctionDec name param Nothing

pFunctionParam :: Parser [FuncParam]
pFunctionParam = try a <|> b  where
    a = do
        a1 <- pVarList
        col
        a2 <- pVarType
        semi
        a3 <- pFunctionParam
        return $ (a1, a2) : a3
    b = do
        b1 <- pVarList
        col
        b2 <- pVarType
        return [(b1, b2)]

---------- PROG BLOCK START ----------
pStatementBlock :: Parser Statement
pStatementBlock = do
    pRW_begin
    stat <- pStatements
    optional semi
    pRW_end
    return $ StatementBlock stat
---------- PROG BLOCK END ----------

pStatements :: Parser [Statement]
pStatements = try a <|> b
  where
    a = do
        a1 <- pStatement
        semi
        a2 <- pStatements
        return (a1 : a2)
    b = do
        b1 <- pStatement
        return [b1]

pStatement :: Parser Statement
pStatement = choice
    [ pStatementBlock
    , pIf
    , pCase
    , pWhileLoop
    , pForLoop
    , pRepeatUntilLoop
    , try pAssign
    , pReadln
    , pWriteln
    , pProcCall
    ]

pIf :: Parser Statement
pIf = do
    expr <- between pRW_if pRW_then pExpr
    s1   <- pStatement
    s2   <- optional $ pRW_else >> pStatement
    return $ StatementIf expr s1 s2

pCase :: Parser Statement
pCase = do
    expr <- between pRW_case pRW_of pExpr
    cls  <- pCaseLines
    optional semi
    ms <- optional ((pRW_else <|> pRW_otherwise) >> pStatement)
    optional semi
    pRW_end
    return $ StatementCase expr cls ms

pCaseLines :: Parser [CaseLine]
pCaseLines = try a <|> b
  where
    a = do
        a1 <- pCaseLine
        semi
        a2 <- pCaseLines
        return (a1 : a2)
    b = do
        b1 <- pCaseLine
        return [b1]

pCaseLine :: Parser CaseLine
pCaseLine = pValueLiteralList >>= \vls -> col >> CaseLine vls <$> pStatement

pWhileLoop :: Parser Statement
pWhileLoop = do
    expr <- between pRW_while pRW_do pExpr
    StatementWhile expr <$> pStatement

pForLoop :: Parser Statement
pForLoop = do
    pRW_for
    i <- identifier
    col >> equ
    ei      <- pExpr
    loopDir <- pForLoopDir
    ef      <- pExpr
    pRW_do
    StatementFor (i, ei) loopDir ef <$> pStatement

pForLoopDir :: Parser ForLoopDirection
pForLoopDir = try (pRW_downto >> return DownTo) <|> (pRW_to >> return To)

pRepeatUntilLoop :: Parser Statement
pRepeatUntilLoop = do
    statements <- between pRW_repeat ((optional semi) >> pRW_until) pStatements
    StatementRepeatUntil statements <$> pExpr

---------- (RE)ASSIGN START ----------
pAssign :: Parser Statement
pAssign = do
    i <- identifier
    col >> equ
    v <- pExpr
    return $ Assign (i, v)
---------- (RE)ASSIGN END ----------

---------- IO START ----------
pReadln :: Parser Statement
pReadln = Readln <$> (pRW_readln >> parens pVarList)

pWriteln :: Parser Statement
pWriteln = Writeln <$> (pRW_writeln >> parens pExprs)
---------- IO END ----------

pProcCall :: Parser Statement
pProcCall = identifier >>= \name ->
    ProcCall name <$> optionalList (parens (optionalList pExprs))

---------- EXPR START ----------
pExprs :: Parser [Expr]
pExprs = try a <|> b  where
    a = do
        a1 <- pExpr
        com
        a2 <- pExprs
        return (a1 : a2)
    b = do
        b1 <- pExpr
        return [b1]

pVarCall :: Parser Expr
pVarCall = identifier >>= \name ->
    VarCall name <$> optionalList (parens (optionalList pExprs))

pInteger :: Parser ValueLiteral
pInteger = Int <$> signedInt

pFloat :: Parser ValueLiteral
pFloat = Double <$> signedDouble

pFalse :: Parser ValueLiteral
pFalse = pRW_false >> return BFalse

pTrue :: Parser ValueLiteral
pTrue = pRW_true >> return BTrue

pStringLiteral :: Parser ValueLiteral
pStringLiteral =
    lexeme
        $   StringLiteral
        .   pack
        <$> (char '\'' *> manyTill L.charLiteral (char '\''))

pValueLiteral :: Parser ValueLiteral
pValueLiteral =
    choice [try pFloat, try pInteger, try pFalse, try pTrue, try pStringLiteral]

pValueLiteralList :: Parser [ValueLiteral]
pValueLiteralList = try a <|> b  where
    a = do
        a1 <- pValueLiteral
        com
        a2 <- pValueLiteralList
        return (a1 : a2)
    b = do
        b1 <- pValueLiteral
        return [b1]

pTerm :: Parser Expr
pTerm = choice
    [parens pExpr, try pValueLiteral >>= \vl -> return $ VExpr vl, pVarCall]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol' name)
prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol' name)
postfix name f = Postfix (f <$ symbol' name)

binaryNotFollowedBy
    :: Text -> Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binaryNotFollowedBy name next f = InfixL (f <$ opNotFollowedBy name next)

opNotFollowedBy :: Text -> Text -> Parser Text
opNotFollowedBy name next =
    (lexeme . try) (symbol' name <* notFollowedBy (symbol' next))

operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [prefix "-" Neg, prefix "+" id]
    , [prefix "~" Not, prefix "not" Not]
    , [binary "&&" And, binary "&" And, binaryNotFollowedBy "and" "then" And]
    , [binary "||" Or, binary "|" Or, binaryNotFollowedBy "or" "else" Or]
    , [binary "^" Xor, binary "xor" Xor]
    , [ binary "<<"  ShiftLeft
      , binary "shl" ShiftLeft
      , binary ">>"  ShiftRight
      , binary "shr" ShiftRight
      ]
    , [binary "*" Mul, binary "/" Div, binary "%" Mod]
    , [binaryNotFollowedBy "+" "+" Sum, binary "-" Sub]
    , [ binary "="  Eq
      , binary "<>" NotEq
      , binary "!=" NotEq
      , binaryNotFollowedBy ">" "=" GreaterThan
      , binaryNotFollowedBy "<" "=" LessThan
      , binary ">=" GreaterThanEq
      , binary "<=" LessThanEq
      ]
    , [ binary "and then" AndThen
      , binary "or else"  OrElse
      ]
    -- string specific operators
    , [binary "++" StringConcat]
    ]
---------- EXPR END ----------

---------- UTIL START ----------
rws :: [Text] -- list of reserved words
rws =
    [ "var"
    , "begin"
    , "end"
    , "program"
    , "boolean"
    , "integer"
    , "real"
    , "string"
    , "false"
    , "true"
    , "writeln"
    , "readln"
    , "if"
    , "then"
    , "else"
    , "case"
    , "otherwise"
    , "of"
    , "while"
    , "for"
    , "do"
    , "to"
    , "downto"
    , "repeat"
    , "until"
    , "function"
    , "procedure"
    , "return"
    ]

pRW_var = rword "var"
pRW_begin = rword "begin"
pRW_end = rword "end"
pRW_program = rword "program"
pRW_boolean = rword "boolean"
pRW_integer = rword "integer"
pRW_real = rword "real"
pRW_string = rword "string"
pRW_false = rword "false"
pRW_true = rword "true"
pRW_writeln = rword "writeln"
pRW_readln = rword "readln"
pRW_if = rword "if"
pRW_then = rword "then"
pRW_else = rword "else"
pRW_case = rword "case"
pRW_otherwise = rword "otherwise"
pRW_of = rword "of"
pRW_while = rword "while"
pRW_for = rword "for"
pRW_do = rword "do"
pRW_downto = rword "downto"
pRW_to = rword "to"
pRW_repeat = rword "repeat"
pRW_until = rword "until"
pRW_function = rword "function"
pRW_procedure = rword "procedure"
pRW_return = rword "return"

rword :: Text -> Parser Text
rword w = if w `elem` rws
    then symbol' w
    else fail $ "keyword " ++ show w ++ " cannot be a reserved word"

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p =
        toLower
            .   pack
            <$> ((:) <$> letterChar <*> many
                    (try alphaNumChar <|> (head . unpack <$> symbol' "_"))
                )
    check x = if x `elem` rws
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

symbol :: Text -> Parser Text
symbol = L.symbol sc

symbol' :: Text -> Parser Text
symbol' = L.symbol' sc

int :: Parser Int
int = lexeme L.decimal

signedInt :: Parser Int
signedInt = L.signed sc int

double :: Parser Double
double = lexeme L.float

signedDouble :: Parser Double
signedDouble = L.signed sc double

semi :: Parser Text
semi = head <$> some (symbol ";")

col :: Parser Text
col = symbol ":"

com :: Parser Text
com = symbol ","

equ :: Parser Text
equ = symbol "="

dot :: Parser Text
dot = symbol "."

sc :: Parser ()
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
---------- UTIL END ----------
