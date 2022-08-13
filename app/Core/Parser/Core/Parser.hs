module Core.Parser.Core.Parser where
  import Core.Parser.AST (Statement(..), Variable(..), Expression(..), Literal(..))
  import Text.Parsec.String (Parser)
  import Core.Parser.Location (Located((:>:)), getLocation)
  import qualified Text.Parsec.Token as Token
  import Text.Parsec.Expr (Operator(..), buildExpressionParser, Assoc(..))
  import Control.Monad.Identity (Identity)
  import Control.Applicative (Alternative (many, some))
  import Text.Parsec (getPosition, optionMaybe, try, sepBy, choice, (<|>), (<?>))
  import Data.Functor (($>))
  import Core.Parser.Core.Lexer
  
  type Xenon a = Parser (Located a)

  parser :: Xenon Statement 
  parser = whiteSpace *> statement

  locate :: Parser a -> Xenon a
  locate p = do
    s <- getPosition
    r <- p
    e <- getPosition
    return $ r :>: (s, e)

  {------------------------}
  
  statement :: Xenon Statement
  statement = choice [
      sFunction,
      sStructure,
      sExtension,
      sReturn,
      sModification,
      sAssignment,
      sSequence,
      sIf,
      sExpression
    ]

  sAssignment :: Xenon Statement
  sAssignment = do
    s <- getPosition
    reserved "var"
    name <- identifier
    value <- optionMaybe $ reservedOp "=" *> expression
    e <- getPosition
    return $ SAssignment name value :>: (s, e)

  sModification :: Xenon Statement
  sModification = do
    s <- getPosition
    name <- try $ variable <* reservedOp "="
    value@(_ :>: (_, e)) <- expression
    return $ SModification name value :>: (s, e)

  sFunction :: Xenon Statement
  sFunction = do
    s <- getPosition
    reserved "func"
    name <- identifier
    args <- parens $ commaSep identifier
    body <- statement
    e <- getPosition
    return $ SFunction name args body :>: (s, e)

  sSequence :: Xenon Statement
  sSequence = do
    s <- getPosition
    statements <- braces $ many statement
    e <- getPosition
    return $ SSequence statements :>: (s, e)

  sStructure :: Xenon Statement
  sStructure = do
    s <- getPosition
    reserved "struct"
    name <- identifier
    fields <- braces $ sepBy statement whiteSpace
    e <- getPosition
    return $ SStructure name fields :>: (s, e)

  sExtension :: Xenon Statement
  sExtension = do
    s <- getPosition
    reserved "extension"
    name <- identifier
    fields <- braces $ sepBy statement whiteSpace
    e <- getPosition
    return $ SExtension name fields :>: (s, e)

  sIf :: Xenon Statement
  sIf = do
    s <- getPosition
    reserved "if"
    condition <- parens expression
    thenBranch <- statement
    elseBranch <- optionMaybe $ do
      reserved "else"
      statement
    e <- getPosition
    return $ SIf condition thenBranch elseBranch :>: (s, e)

  sReturn :: Xenon Statement
  sReturn = do
    s <- getPosition
    reserved "return"
    expr <- expression
    e <- getPosition
    return $ SReturn expr :>: (s, e)

  sExpression :: Xenon Statement
  sExpression = do
    expr :>: pos <- expression
    return $ SExpression expr :>: pos

  {------------------------}

  variable :: Xenon Variable
  variable = buildExpressionParser table' vVariable
    where table' = [
              [Postfix $ makeUnaryOp postfix]
            ]
          postfix = object <|> index
          object = do
            reservedOp "."
            object <- identifier
            e <- getPosition
            return $ \x@(_ :>: (_, s)) -> VObject x object :>: (e, s)
          index = do
            index' <- Token.brackets lexer expression
            e <- getPosition
            return $ \x@(_ :>: (p, _)) -> VArray x index' :>: (p, e)
  
  vVariable :: Xenon Variable
  vVariable = do
    s <- getPosition
    name <- identifier
    return $ VVariable name :>: (s, s)


  {------------------------}

  expression :: Xenon Expression 
  expression = buildExpressionParser table term

  term :: Xenon Expression
  term = choice [
      eLiteral <?> "literal",
      eSelf <?> "self reference",
      eVariable <?> "variable",
      parens expression <?> "expression"
    ]

  eVariable :: Xenon Expression
  eVariable = do
    s <- getPosition
    name <- identifier
    e <- getPosition
    return $ EVariable name :>: (s, e)

  eLambda :: Xenon Expression
  eLambda = do
    s <- getPosition
    reserved "func"
    args <- parens $ commaSep identifier
    body <- statement
    e <- getPosition
    return $ ELambda args body :>: (s, e)

  eSelf :: Xenon Expression
  eSelf = do
    s <- getPosition
    reserved "self"
    e <- getPosition
    return $ ESelf :>: (s, e)

  eLiteral :: Xenon Expression
  eLiteral = lInteger <|> lString <|> lBool <|> lFloat <|> lNullable

  {------------------------}

  lInteger :: Xenon Expression
  lInteger = do
    s <- getPosition
    i <- integer
    e <- getPosition
    return $ ELiteral (LInteger i) :>: (s, e)
  
  lString :: Xenon Expression
  lString = do
    s <- getPosition
    str <- Token.stringLiteral lexer
    e <- getPosition
    return $ ELiteral (LString str) :>: (s, e)

  lFloat :: Xenon Expression
  lFloat = do
    s <- getPosition
    f <- Token.float lexer
    e <- getPosition
    return $ ELiteral (LFloat f) :>: (s, e)
  
  lBool :: Xenon Expression
  lBool = do
    s <- getPosition
    b <- (reserved "true" $> True) <|> (reserved "false" $> False)
    e <- getPosition
    return $ ELiteral (LBoolean b) :>: (s, e)

  lNullable :: Xenon Expression
  lNullable = do
    s <- getPosition
    reserved "nil"
    e <- getPosition
    return $ ELiteral LNullable :>: (s, e)

  {------------------------}

  makeUnaryOp :: Alternative f => f (a -> a) -> f (a -> a)
  makeUnaryOp s = foldr1 (.) . reverse <$> some s

  table :: [[Operator String () Identity (Located Expression)]]
  table = [
      [Postfix $ makeUnaryOp postfix],
      equalities,
      [Infix (reserved "is" >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EIs x y :>: (s, e)) <?> "type condition") AssocNone],
      [Postfix $ do
        reserved "?"
        thn <- expression
        reserved ":"
        els <- expression
        return (\x@(_ :>: (p, _)) -> ETernary x thn els :>: (p, snd $ getLocation els))],
      [Infix (reservedOp "*" >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "*" x y :>: (s, e))) AssocLeft,
       Infix (reservedOp "/" >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "/" x y :>: (s, e))) AssocLeft],
      [Infix (reservedOp "+" >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "+" x y :>: (s, e))) AssocLeft,
       Infix (reservedOp "-" >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary "-" x y :>: (s, e))) AssocLeft]
    ]
    where postfix = (call <?> "function call") <|> (object <?> "struct property") <|> (index <?> "array index")
          call = do
            args <- parens $ commaSep expression
            e <- getPosition
            return $ \x@(_ :>: (s, _)) -> ECall x args :>: (s, e)
          object = do
            reservedOp "."
            object <- identifier
            e <- getPosition
            return $ \x@(_ :>: (_, s)) -> EProperty x object :>: (e, s)
          index = do
            index' <- Token.brackets lexer expression
            e <- getPosition
            return $ \x@(_ :>: (p, _)) -> EIndex x index' :>: (p, e)

          -- Equality operators
          equalityOp = ["==", "!=", "<", ">", "<=", ">="]
          equalities = map (\op -> Infix (reservedOp op >> return (\x@(_ :>: (s, _)) y@(_ :>: (_, e)) -> EBinary op x y :>: (s, e))) AssocLeft) equalityOp
