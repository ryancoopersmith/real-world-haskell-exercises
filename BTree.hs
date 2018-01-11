import Data.Maybe

data BTree a = Node (Maybe a) (Maybe (BTree a)) (Maybe (BTree a))
    deriving (Eq, Show)

bTreeHeight :: Eq a => BTree a -> Int
bTreeHeight (Node node child1 child2)
    | node   == Nothing = 0
    | child1 == Nothing && child2 == Nothing = nodeHeight -- node exists but no children, height = 1
    | child1 == Nothing = nodeHeight + child2Height -- we know child2 exists and the node exists so height = 1 + (height of child)
    | child2 == Nothing = nodeHeight + child1Height -- we know child1 exists and the node exists so height = 1 + (height of child)
    | otherwise = if child1Height >= child2Height -- both children exist so height = 1 + (height of child w/ most children)
                  then nodeHeight + child1Height
                  else nodeHeight + child2Height
    where nodeHeight = 1
          child1Height = bTreeHeight (fromJust child1)
          child2Height = bTreeHeight (fromJust child2)


-- testing it out: bTreeHeight
-- (Node
    -- (Just "root")
    -- (Just (Node
        -- (Just "child")
        -- (Just (Node
            -- (Just "grandchild")
            -- Nothing
            -- Nothing))
        -- Nothing))
    -- (Just (Node
    -- (Just "child")
    -- Nothing
    -- Nothing))
-- )
-- height should be 3

-- bTreeHeight (Node (Just "root") (Just (Node (Just "child") (Just (Node (Just "grandchild") Nothing Nothing)) Nothing)) (Just (Node (Just "child") Nothing Nothing)))
