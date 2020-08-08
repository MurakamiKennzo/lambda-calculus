module Lambda.Parser
  (
    parseLambda
  ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Token
import Data.Functor.Identity
import Lambda.Syntax
    
lambdaLanguage :: LanguageDef st
lambdaLanguage = emptyDef { Token.reservedOpNames = ["λ", "."] }

lexer :: Token.GenTokenParser String st Identity
lexer = Token.makeTokenParser lambdaLanguage

identifier :: Parser String
identifier = Token.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

variable :: Parser Lambda
variable = identifier >>= return . Variable

abstraction :: Parser Lambda
abstraction = do
  reservedOp "λ"
  v <- identifier
  reservedOp "."
  body <- lambda
  return $ Abstraction v body

expr :: Parser Lambda
expr = parens lambda
     <|> abstraction
     <|> variable

lambda :: Parser Lambda
lambda = many1 expr >>= return .foldl1 Application

parseLambda :: String -> Either ParseError Lambda
parseLambda = parse ((whiteSpace >> lambda) <* eof) "<stdin>"
