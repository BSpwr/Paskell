{-# LANGUAGE OverloadedStrings #-}

module Parser where


import           Control.Applicative
import           Control.Monad
import           Data.Text                      ( Text
                                                , unpack
                                                , pack
                                                , toLower
                                                )
import           Data.Maybe                     ( maybe
                                                , Maybe(..)
                                                )
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           System.Environment
import           Control.Monad.Combinators.Expr
import qualified Data.Text                     as T
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char.Lexer    as L

import           TypeAST

type Parser = Parsec Void Text

pRun :: Parser AST
pRun = pStart <* eof

pStart :: Parser AST
pStart = do
    pProgramHeader
    a1 <- pVarBlocks
    semi
    a2 <- pProgBlock
    dot
    return $ Node "Root" (VarBlock a1) (ProgBlock a2)

pProgramHeader :: Parser Text
pProgramHeader = try (rword "program" >> identifier) <|> rword "program"

---------- VAR BLOCK WITH ASSIGNMENT START ----------
pVarBlocks :: Parser [VarDef]
pVarBlocks = try a <|> b  where
    a = do
        rword "var"
        a1 <- pVarDefs
        semi
        rword "var"
        a2 <- pVarDefs
        return (a1 ++ a2)
    b = rword "var" >> pVarDefs

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
        a3 <- pGenExpr
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
    (rword "boolean" >> return BoolType)
        <|> (rword "integer" >> return IntType)
        <|> (rword "real" >> return RealType)
        <|> (rword "string" >> return StringType)
        <|> (EnumType . unpack <$> identifier)
---------- VAL BLOCK WITH ASSIGNMENT END ----------

---------- PROG BLOCK START ----------
pProgBlock :: Parser [Impl]
pProgBlock = do
    rword "begin"
    stat <- pStatements
    optional semi
    rword "end"
    return stat
---------- PROG BLOCK END ----------

pStatements :: Parser [Impl]
pStatements = try a <|> b
  where
    a = do
        a1 <- pImplementation
        semi
        a2 <- pStatements
        return (a1 : a2)
    b = do
        b1 <- pImplementation
        return [b1]

pImplementation :: Parser Impl
pImplementation = choice [try pAssign, pReadln, pWriteln]

---------- (RE)ASSIGN START ----------
pAssign :: Parser Impl
pAssign = do
    i <- identifier
    col >> equ
    v <- pGenExpr
    return $ Assign (i, v)
---------- (RE)ASSIGN END ----------

---------- IO START ----------
pReadln :: Parser Impl
pReadln = do
    rword "readln"
    symbol "("
    vL <- pVarList
    symbol ")"
    return $ Readln vL

pWriteln :: Parser Impl
pWriteln = do
    rword "writeln"
    symbol "("
    ex <- pGenExprs
    symbol ")"
    return $ Writeln ex
---------- IO END ----------

---------- NUM EXPR START ----------
pGenExprs :: Parser [GenExpr]
pGenExprs = try a <|> b  where
    a = do
        a1 <- pGenExpr
        com
        a2 <- pGenExprs
        return (a1 : a2)
    b = do
        b1 <- pGenExpr
        return [b1]

pGenExpr :: Parser GenExpr
pGenExpr = try s <|> try b <|> n  where
    s = StringExpr <$> pStringExpr
    b = BoolExpr <$> pBoolExpr
    n = NumExpr <$> pNumExpr

pNumVar :: Parser NumExpr
pNumVar = NVar <$> identifier

pInteger :: Parser NumExpr
pInteger = Int <$> signedInt

pFloat :: Parser NumExpr
pFloat = Double <$> signedDouble

pNumTerm :: Parser NumExpr
pNumTerm = choice [parens pNumExpr, try pFloat, pInteger, pNumVar]

pNumExpr :: Parser NumExpr
pNumExpr = makeExprParser pNumTerm numOperatorTable

binaryN :: Text -> (NumExpr -> NumExpr -> NumExpr) -> Operator Parser NumExpr
binaryN name f = InfixL (f <$ symbol name)
prefixN, postfixN :: Text -> (NumExpr -> NumExpr) -> Operator Parser NumExpr
prefixN name f = Prefix (f <$ symbol name)
postfixN name f = Postfix (f <$ symbol name)

numOperatorTable :: [[Operator Parser NumExpr]]
numOperatorTable =
    [ [prefixN "-" Neg, prefixN "+" id]
    , [binaryN "*" Mul, binaryN "/" Div]
    , [binaryN "+" Sum, binaryN "-" Sub]
    ]
---------- NUM EXPR END ----------

---------- BOOL EXPR START ----------
pBoolVar :: Parser BoolExpr
pBoolVar = BVar <$> identifier

pFalse :: Parser BoolExpr
pFalse = do
    rword "false"
    return BFalse

pTrue :: Parser BoolExpr
pTrue = do
    rword "true"
    return BTrue

pBoolTerm :: Parser BoolExpr
pBoolTerm = choice [parens pBoolExpr, try pFalse, try pTrue, pBoolVar]

pBoolExpr :: Parser BoolExpr
pBoolExpr = makeExprParser pBoolTerm boolOperatorTable

binaryB
    :: Text -> (BoolExpr -> BoolExpr -> BoolExpr) -> Operator Parser BoolExpr
binaryB name f = InfixL (f <$ symbol name)
prefixB, postfixB :: Text -> (BoolExpr -> BoolExpr) -> Operator Parser BoolExpr
prefixB name f = Prefix (f <$ symbol name)
postfixB name f = Postfix (f <$ symbol name)

boolOperatorTable :: [[Operator Parser BoolExpr]]
boolOperatorTable =
    [[prefixB "~" Not], [binaryB "||" Or, binaryB "&&" And], [binaryB "^" Xor]]
---------- BOOL EXPR END ----------

---------- STRING EXPR START ----------
pStringVar :: Parser StringExpr
pStringVar = SVar <$> identifier

pStringLiteral :: Parser StringExpr
pStringLiteral =
    lexeme
        $   StringE
        .   pack
        <$> (char '\'' *> manyTill L.charLiteral (char '\''))

pStringTerm :: Parser StringExpr
pStringTerm = choice [parens pStringExpr, pStringLiteral, pStringVar]

pStringExpr :: Parser StringExpr
pStringExpr = makeExprParser pStringTerm stringOperatorTable

binaryS
    :: Text
    -> (StringExpr -> StringExpr -> StringExpr)
    -> Operator Parser StringExpr
binaryS name f = InfixL (f <$ symbol name)
prefixS, postfixS
    :: Text -> (StringExpr -> StringExpr) -> Operator Parser StringExpr
prefixS name f = Prefix (f <$ symbol name)
postfixS name f = Postfix (f <$ symbol name)

stringOperatorTable :: [[Operator Parser StringExpr]]
stringOperatorTable = [[binaryS "++" Concat]]
---------- STRING EXPR END ----------

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
    ]

rword :: Text -> Parser Text
rword w = if w `elem` rws
    then symbol' w
    else fail $ "keyword " ++ show w ++ " cannot be a reserved word"

identifier :: Parser Text
identifier = (lexeme . try) (p >>= check)
  where
    p = toLower . pack <$> ((:) <$> letterChar <*> M.many alphaNumChar)
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
semi = symbol ";" >> head <$> M.many semi

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
