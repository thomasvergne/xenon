module Core.Parser.Location where
  import Text.Parsec (SourcePos)
  
  data Located a = a :>: (SourcePos, SourcePos)
  
  instance Show a => Show (Located a) where
    show (a :>: _) = show a
  
  getLocation :: Located a -> (SourcePos, SourcePos)
  getLocation (_ :>: pos) = pos