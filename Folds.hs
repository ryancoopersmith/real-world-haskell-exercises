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

-- General gist of using Either instead of error; not working, need knowledge of Modads first to proceed
-- type ErrorMessage = String
-- asInt_either :: String -> Either ErrorMessage Int
-- asInt_either xs = if "-" `isPrefixOf` xs
--                 then negate (foldl' step 0 xs)
--                 else foldl' step 0 xs
--     where step y x
--             | x == '-' = Right (y)
--             | not (isHexDigit x) = Left ([x] ++ " is not a (-) or hex digit!!!")
--             | otherwise =  Right (y * 10 + digitToInt x)

concat_fold :: [[a]] -> [a]
concat_fold xs = foldr step [] xs
    where step x ys
            | null x = ys
            | otherwise = x ++ ys

takeWhile_explicit :: (a -> Bool) -> [a] -> [a]
takeWhile_explicit p (x:xs)
    | p x = x : (takeWhile_explicit p xs)
    | otherwise = []
takeWhile_explicit _ _ = []

takeWhile_fold :: (a -> Bool) -> [a] -> [a]
takeWhile_fold _ [] = []
takeWhile_fold p xs = foldr step [] xs
    where step x ys
            | p x = x:ys
            | otherwise = []
-- 'ys' really is the list of remaining 'step' thunks, mapped for each element in the list
-- since we're using lazy evaluation and foldr, not 'calling' ys will not evaluate the rest of the 'ys' thunk, resulting in a base case
-- (at least this is my take on it); folds are tough, read http://www.cs.nott.ac.uk/~pszgmh/fold.pdf and https://en.wikipedia.org/wiki/Fold_(higher-order_function) to get a better grasp on them

-- NOTE: fold associativity: fold(l/r) (-) 0 [1,2,3]
-- * foldr: (1-(2-(3-0))) = 2
-- * foldl: (((0-1)-2)-3) = -6

-- groupBy ex: (groupBy test [1,1,2,4,5,5,6,1]) == [[1,1],[2],[4],[5,5],[6],[1]]
-- test x y = x == y

groupBy_fold :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_fold _ [] = []
groupBy_fold p xs = foldr step [] xs
    where step x [] = [[x]]
          step x (y:ys)
            | p x (head y) = (x:y):ys
            | otherwise = [x]:(y:ys)

-- For the following functions, explain which is more appropriate: foldl' or foldr

any_fold :: (a -> Bool) -> [a] -> Bool
any_fold _ [] = False
any_fold p xs = foldr step False xs
    where step _ True = True
          step x _ = p x

-- cycle_fold :: [a] -> [a]
-- cycle_fold xs = foldr step xs xs
-- where step x xs = 

-- words_fold :: String -> [String]
-- words_fold s = foldr step [] s
--     where step c (x:xs)
--             | c == ' ' = c:(x:xs)
--             | otherwise = (c:x):xs

-- unlines_fold :: [String] -> String
