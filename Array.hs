import Data.Array

-- | Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s a = go s (indices a)
    where go s (j:js) = let s' = f s (a ! j)
                        in s' `seq` go s' js
          go s _ = s

-- | Strict left fold using the first element of the array as its
-- starting value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a

-- demonstrating the suitability of tuples as stand-ins for arrays (not very suitable)
-- all args need to be of same type (create custom data type to get around this)
(!!!) :: (a, a, a, a) -> Int -> a
(a, b, c, d) !!! i | i == 1 = a
                   | i == 2 = b
                   | i == 3 = c
                   | i == 4 = d

-- Arrays provide MUCH cleaner + faster element lookup
