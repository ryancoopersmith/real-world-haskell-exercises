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
-- since we're using foldr, the thunk we're building up in the stack ends when we stop using the accumulator, effectively short circuting the fold
-- foldl would have just used '[]' in the accumulator, continuing the fold
-- folds resources: http://www.cs.nott.ac.uk/~pszgmh/fold.pdf and https://en.wikipedia.org/wiki/Fold_(higher-order_function) and https://wiki.haskell.org/Foldr_Foldl_Foldl%27

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

-- can use either foldr or foldl' here since associativity is not an issue
-- would use foldr since more ubiquitous in real scenario; just using foldl' here for example
any_fold :: (a -> Bool) -> [a] -> Bool
any_fold _ [] = False
any_fold p xs = foldl' step False xs
    where step True _ = True
          step _ x = p x

-- had to use foldr since infinite
cycle_fold :: [a] -> [a]
cycle_fold [] = error "You gave me an empty list!"
cycle_fold xs = foldr (:) (cycle_fold xs) xs

-- using foldr here to avoid reversing list
words_fold :: String -> [String]
words_fold s = foldr step [] s
    where step c []
            | c == ' ' = []
            | otherwise = (c:[]):[]
          step c (x:xs)
            | c == ' ' = []:(x:xs)
            | otherwise = (c:x):xs

-- using foldr here to avoid reversing list
unlines_fold :: [String] -> String
unlines_fold xs = foldr step [] xs
    where step s ys = (s ++ "\n") ++ ys
