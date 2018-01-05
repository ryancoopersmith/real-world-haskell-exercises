import Data.List

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) | null xs   = 1
                | otherwise = length + myLength xs
              where length  = 1

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myMean :: [Int] -> Double
-- need to check for empty list here as well to prevent division by 0
myMean xs | null xs      = 0.0
          | otherwise    = (fromIntegral listSum) / (fromIntegral listLength)
        where listSum    = mySum xs
              listLength = myLength xs

reverseList :: [a] -> [a]
reverseList xs | null (tail xs) = xs
               | otherwise      = [last xs] ++ (reverseList (init xs))

palindrome :: [a] -> [a]
palindrome xs | null xs || null (tail xs) = xs -- empty or single item list is a palindrome
              | otherwise                 = xs ++ (reverseList xs)

lengthSort :: [a] -> [a] -> Ordering
lengthSort xs1 xs2 | myLength xs1 <= myLength xs2 = LT
                   | myLength xs1 > myLength xs2  = GT

sortLOL :: [[a]] -> [[a]] -- sort list of lists by length of sublist
sortLOL xs = sortBy lengthSort xs

joinLOL :: Char -> [String] -> String -- join list of lists by seperator
joinLOL s xs | null xs   = ""
             | otherwise = (head xs) ++ [s] ++ joinLOL s (tail xs)
