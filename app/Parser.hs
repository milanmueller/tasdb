module Parser (
  Cmd (..),
  ValueCmd (..),
  MetricCmd (..),
  Type (..),
  Val (..),
  parseCmd,
)
where

import Control.Applicative (Alternative ((<|>)))
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token

-- AST
data Val = VStr String | VInt Int | VBool Bool deriving (Show)
data Type = TInt | TBool | TEnum [String] deriving (Show)
data MetricCmd = Add String Type deriving (Show)
data ValueCmd = Insert String Val deriving (Show)
data Cmd = MCmd MetricCmd | VCmd ValueCmd deriving (Show)

-- Lexer
lexer :: TokenParser ()
lexer = makeTokenParser style
  where
    style =
      emptyDef
        { reservedNames = ["metric", "value", "insert", "int", "bool", "enum", "true", "false"]
        , reservedOpNames = ["[", "]", ","]
        }

-- Parsers
parseVal :: Parser Val
parseVal = parseStr <|> parseInt
  where
    parseStr = parseTrue <|> parseFalse <|> VStr <$> stringLiteral lexer
      where
        parseTrue = reserved lexer "true" >> return (VBool True)
        parseFalse = reserved lexer "false" >> return (VBool False)
    parseInt = VInt . fromInteger <$> integer lexer

parseType :: Parser Type
parseType = parseIntType <|> parseBoolType <|> parseEnumType
  where
    parseIntType = reserved lexer "int" >> return TInt
    parseBoolType = reserved lexer "bool" >> return TBool
    parseEnumType = do
      reserved lexer "enum"
      enumValues <- brackets lexer $ commaSep1 lexer $ stringLiteral lexer
      return $ TEnum enumValues

parseMetricCmd :: Parser MetricCmd
parseMetricCmd = do
  reserved lexer "metric"
  parseMetricAdd
  where
    parseMetricAdd = do
      reserved lexer "add"
      mname <- stringLiteral lexer
      Add mname <$> parseType

parseValueCmd :: Parser ValueCmd
parseValueCmd = do
  reserved lexer "value"
  reserved lexer "insert"
  mname <- stringLiteral lexer
  Insert mname <$> parseVal

parseCmd :: Parser Cmd
parseCmd = (MCmd <$> parseMetricCmd) <|> (VCmd <$> parseValueCmd)
