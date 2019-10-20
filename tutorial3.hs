import Prelude hiding (filter)
-- q1
f :: [Int] -> Bool
f = (> 100) . sum . map ((+1) . (*2)) . filter even

--q4
filter :: (a -> Bool) -> [a] -> [a]
filter p = flip foldr [] $ \x xs -> if p x then x:xs else xs

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)
-- q5

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf 
treeMap f (Node l n r) = Node (nl) (f n) (nr)
    where nl = treeMap f l
          nr = treeMap f r

-- q6
-- foldTree :: (a -> b -> b) -> b -> Tree a -> b
-- foldTree f v 