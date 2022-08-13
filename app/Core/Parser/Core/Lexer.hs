module Core.Parser.Core.Lexer where 
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Token (GenTokenParser)
  import Control.Monad.Identity (Identity)
  import Text.Parsec.Language (emptyDef)
  import Text.Parsec (letter, alphaNum, oneOf)
  import Control.Applicative (Alternative((<|>)))
  import Text.Parsec.String (Parser)
  
  languageDef =
    emptyDef {  Token.commentStart    = "/*"
              , Token.commentEnd      = "*/"
              , Token.commentLine     = "//"
              , Token.identStart      = letter
              , Token.identLetter     = alphaNum <|> oneOf "_$"
              , Token.reservedNames   = ["var", "func", "if", "struct", "extension", "return", "is"]
              , Token.reservedOpNames = ["(", ")", "*", "+", "-", "/", "{", "}", "[", "]", "="] }

  lexer :: GenTokenParser String u Identity
  lexer = Token.makeTokenParser languageDef

  identifier :: Parser String
  identifier = Token.identifier lexer

  reserved :: String -> Parser ()
  reserved = Token.reserved lexer

  reservedOp :: String -> Parser ()
  reservedOp = Token.reservedOp lexer

  parens :: Parser a -> Parser a
  parens = Token.parens lexer

  brackets :: Parser a -> Parser a
  brackets = Token.brackets lexer

  braces :: Parser a -> Parser a
  braces = Token.braces lexer

  integer :: Parser Integer
  integer = Token.integer lexer

  whiteSpace :: Parser ()
  whiteSpace = Token.whiteSpace lexer

  comma :: Parser String
  comma = Token.comma lexer

  commaSep :: Parser a -> Parser [a]
  commaSep = Token.commaSep lexer

  semi :: Parser String
  semi = Token.semi lexer