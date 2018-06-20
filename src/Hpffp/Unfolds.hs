module Hpffp.Unfolds where

myIterate :: (a -> a) -> a -> [a]
myIterate f x = [x] ++ (myIterate f (f x))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
    let result = (f x)
     in case result of
            Nothing -> []
            Just (a, b) -> [a] ++ (myUnfoldr f b)

data BinaryTree a
    = Leaf
    | Node (BinaryTree a)
           a
           (BinaryTree a)
    deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f x =
    let result = (f x)
     in case result of
            Nothing -> Leaf
            Just (a, b, c) -> Node (unfoldTree f a)
                                   b
                                   (unfoldTree f c)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (\x -> if x >= n
                                then Nothing
                                else (Just (x+1, x, x+1))) 0
