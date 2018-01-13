data TCTest a = TC a | TCT a

class SpecialCombo a where
    (=?/=), (=?=) :: a -> a -> Bool

    (=?/=) x y = not ((=?=) x y)
    (=?=) x y  = not ((=?/=) x y)

instance SpecialCombo (TCTest a) where
    (=?=) (TC _) (TCT _) = True
    (=?=) _ _            = False

instance SpecialCombo Char where
    (=?=) x y = x == y && x == 'z'

instance Show (TCTest a) where
    show (TC _) = "TC"
    show (TCT _) = "TCT"

data Names = First | Middle | Last

instance Show Names where
    show First  = "First"
    show Middle = "Middle"
    show Last   = "Last"

instance Read Names where
    -- readsPrec is the main function for parsing input
    readsPrec _ value =
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse [("First", First), ("Middle", Middle), ("Last", Last)]
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                      -- Compare the start of the string to be parsed to the
                      -- text we are looking for.
                      if (take (length attempt) value) == attempt
                         -- If we have a match, return the result and the
                         -- remaining input
                         then [(result, drop (length attempt) value)]
                         -- If we don't have a match, try the next pair
                         -- in the list of attempts.
                         else tryParse xs
