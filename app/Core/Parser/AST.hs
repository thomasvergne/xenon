module Core.Parser.AST where
  import Core.Parser.Location (Located)

  data Statement
    = SStructure String [Located Statement]
    | SFunction String [String] (Located Statement)
    | SSequence [Located Statement]
    | SAssignment String (Located Expression)
    | SModification (Located Variable) (Located Expression)
    | SExtension String [Located Statement]
    | SExpression Expression
    | SReturn (Located Expression)
    | SIf (Located Expression) (Located Statement) (Maybe (Located Statement))
    deriving Show

  data Variable
    = VVariable String
    | VObject (Located Variable) String
    | VArray (Located Variable) (Located Expression)
    deriving Show

  data Expression 
    = EVariable String
    | EArray [Located Expression]
    | EIndex (Located Expression) (Located Expression)
    | ECall (Located Expression) [Located Expression]
    | EBinary String (Located Expression) (Located Expression)
    | EProperty (Located Expression) String
    | EUnary String (Located Expression)
    | ELiteral Literal
    | ESelf
    | ETernary (Located Expression) (Located Expression) (Located Expression)
    deriving Show

  data Literal
    = LString String
    | LInteger Integer
    | LFloat Double
    | LBoolean Bool
    deriving Show