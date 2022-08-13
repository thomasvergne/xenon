module Core.Parser.Core where
  import Text.Parsec (ParseError, runParser, many, eof)
  import Core.Parser.Location (Located)
  import Core.Parser.AST (Statement)
  import Core.Parser.Core.Parser (parser)
  
  parseXenon :: String -> String -> Either ParseError [Located Statement]
  parseXenon = runParser (many parser <* eof) ()