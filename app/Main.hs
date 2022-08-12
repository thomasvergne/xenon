module Main where
  import Core.Parser.Parser
  main :: IO ()
  main = do
    x <- readFile "examples/main.xe"
    print $ parseXenon "examples/main.xe" x
