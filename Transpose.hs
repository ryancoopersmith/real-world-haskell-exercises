import System.Environment (getArgs)

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
        myFunction = transpose

transpose :: String -> String
transpose input = unlines (transpose' (lines input))

transpose' :: [String] -> [String]
transpose' (x:xs) | null (tail xs) = []
                  | otherwise = let transposed = transposeChars (x, head (tail xs))
                    in (fst transposed) : (snd transposed) : (transpose' xs)

transposeChars :: (String, String) -> (String, String)
transposeChars ((x:xs), (y:ys))
    | null xs || null ys = ((y:xs), (x:ys))
    | otherwise = (([y] ++ fst transposed), ([x] ++ snd transposed))
    where transposed = transposeChars (tail xs, tail ys)
