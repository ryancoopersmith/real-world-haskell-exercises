import Data.Char (digitToInt, isHexDigit)
import Data.Foldable (foldl') -- NOTE: this ONLY imports foldl' from Data.Foldable
import Data.List (isPrefixOf)

asInt :: String -> Int
asInt xs = loop 0 xs
    where loop acc []     = acc
          loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                            in loop acc' xs

 -- using foldl' instead of foldl to prevent space leaks due to large thunks
asInt_fold :: String -> Int
asInt_fold xs = if "-" `isPrefixOf` xs
                then negate (foldl' step 0 xs)
                else foldl' step 0 xs
    where step y x
            | x == '-' = y
            | not (isHexDigit x) = error ([x] ++ " is not a (-) or hex digit!!!")
            | otherwise =  y * 10 + digitToInt x
