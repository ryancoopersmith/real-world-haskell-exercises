import Data.List
import Data.Maybe

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

myNull :: [a] -> Bool
myNull [] = True
myNull _ = False

myHead :: [a] -> a
myHead (x:_) = x

myTail :: [a] -> [a]
myTail (_:xs) = xs

myLast :: [a] -> a
myLast (x:xs) = if null xs
                then x
                else myLast xs

myInit :: [a] -> [a]
myInit (x:xs) = x : if null (tail xs)
                    then []
                    else myInit xs

myAppend :: [a] -> [a] -> [a]
xs `myAppend` ys = if null xs
                   then ys
                   else (head xs) : myAppend (tail xs) ys

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myAnd :: [Bool] -> Bool
myAnd [] = True -- must default to True since False will always make this false
myAnd (x:xs) = x && (myAnd xs)

myOr :: [Bool] -> Bool
myOr [] = False -- must default to False since True will always make this true
myOr (x:xs) = x || (myOr xs)

myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll pred (x:xs) = (pred x) && (myAll pred xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny pred (x:xs) = (pred x) || (myAny pred xs)

-- to test:
greaterThan3 x = x > 3
-- myAll greaterThan3 [4,5,6] is true
-- myAny greaterThan3 [3,5,1] is true

-- total versions of standard partial list functions
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x : fromJust (safeInit xs))
