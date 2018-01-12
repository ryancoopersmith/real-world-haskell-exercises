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
