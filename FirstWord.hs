import System.Environment (getArgs)
import Data.Maybe

-- compiling: run ghc --make to create the "FirstWord" binary

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- change this assignment to any function that modifies the input
        myFunction = firstWords

firstWords :: String -> String
firstWords input = unlines (firstWords' (lines input))

firstWords' :: [String] -> [String]
firstWords' [] = []
firstWords' (x:xs) = firstWord : (firstWords' xs)
   where firstWord = fromJust (safeHead (words x))

safeHead :: [String] -> Maybe String
safeHead [] = Nothing
safeHead xs = Just (head xs)
