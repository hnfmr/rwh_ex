-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g., Interact.hs

import System.Environment (getArgs)

interactWith function inputFile = do
  input <- readFile inputFile
  function (splitLines input)

splitLines [] = []
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of
             ('\r':'\n':rest) -> splitLines rest
             ('\r':rest)      -> splitLines rest
             ('\n':rest)      -> splitLines rest
             _                -> []

isLineTerminator c = c == '\r' || c == '\n'

printLines :: [String] -> IO ()
printLines (l:ls) = do
                      () <- putStrLn (head $ words $ l)
                      printLines ls

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> interactWith function input
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = printLines