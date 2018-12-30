module Parser where

import Data.Functor.Identity (Identity)

import Text.Parsec
import Text.Parsec.String

import qualified Text.Parsec.Expr as Exp
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

table = [
    [ binary "*" Times Exp.AssocRight,
      binary "/" Divide Exp.AssocRight ]
  , [ binary "+" Plus Exp.AssocRight,
      binary "-" Minus Exp.AssocRight ]
  ]

binary  :: String -> Op -> Exp.Assoc -> Exp.Operator String () Identity Expr
binary name op assoc = Exp.Infix (reservedOp name >> return (BinOp op)) assoc

int :: Parser Expr
int = (Float . fromInteger) <$> integer

floating :: Parser Expr
floating = Float <$> float

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
  reserved "def"
  name <- identifier
  args <- parens $ many variable
  body <- expr
  return $ Function name args body

extern :: Parser Expr
extern = Extern
  <$> (reserved "extern" *> identifier)
  <*> (parens $ many variable)

call :: Parser Expr
call = Call <$> identifier <*> (parens $ commaSep expr)

factor :: Parser Expr
factor = try floating
  <|> try int
  <|> try extern
  <|> try function
  <|> try call
  <|> try call
  <|> variable
  <|> parens expr

expr :: Parser Expr
expr = Exp.buildExpressionParser table factor

defn :: Parser Expr
defn = try extern
  <|> try function
  <|> expr

contents :: Parser a -> Parser a
contents p = Tok.whiteSpace lexer *> p <* eof

toplevel :: Parser [Expr]
toplevel = many (defn <* reservedOp ";")

parseExpr :: String -> Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String -> Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s
