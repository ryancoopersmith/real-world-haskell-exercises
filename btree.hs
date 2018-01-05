import Data.Maybe

data BTree a = Node (Maybe a) (Maybe (BTree a)) (Maybe (BTree a))
    deriving (Eq, Show)

bTreeHeight :: Eq a => BTree a -> Int
bTreeHeight (Node node child1 child2)
    | node   == Nothing = 0
    | child1 == Nothing && child2 == Nothing = 1
    | child1 == Nothing = bTreeHeight (fromJust child2)
    | child2 == Nothing = bTreeHeight (fromJust child1)
    | otherwise = let height = 1
                      child1Height = bTreeHeight (fromJust child1)
                      child2Height = bTreeHeight (fromJust child2)
                  in if child1Height >= child2Height
                     then height + child1Height
                     else height + child2Height

-- testing it out: bTreeHeight (Node (Just "root") (Just (Node (Just "child") Just (Node (Just "grandchild") Nothing Nothing))) Just (Node (Just "child") Nothing Nothing))
-- height should be 3
