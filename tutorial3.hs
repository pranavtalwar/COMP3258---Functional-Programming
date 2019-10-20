import Prelude hiding (filter)
import Text.Printf

--q1
f :: [Int] -> Bool
f = (>100) .sum . map ((+1). (*2)) . filter even


-- mapSqrt :: [Int] -> [Int]
-- mapSqrt = map $ \x -> 
--     if x < 0 
--         then 0 
--     else sqrt x

-- q4
filter :: (a -> Bool) -> [a] -> [a]
filter p = flip foldr [] $ \x xs -> if p x then x:xs else xs 

--q5 
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Leaf = Leaf
treeMap f (Node l n r) = Node (treeMap f l) (f n) (treeMap f r)

--q6
foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree f v (Leaf) = v
foldTree f v (Node l n r) = foldTree f (f n (foldTree f v r)) l

--

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

eval :: Expr -> Int 
eval (Val n) = n
eval (Add expr1 expr2) = eval(expr1) + eval(expr2)
eval (Mul expr1 expr2) = eval(expr1) * eval(expr2)

foldExpr :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr v _ _ (Val n) = v n 
foldExpr v a m (Add expr1 expr2) = a (foldExpr v a m expr1) (foldExpr v a m expr2)
foldExpr v a m (Mul expr1 expr2) = m (foldExpr v a m expr1) (foldExpr v a m expr2)

binary :: String -> String -> String -> String 
binary op x y = printf "(%s %s %s)" x op y

printExpr :: Expr -> String 
printExpr (Val n) = show n
printExpr (Add x y) = binary "+" (printExpr x) (printExpr y)
printExpr (Mul x y) = binary "*" (printExpr x) (printExpr y)

--q7
collect :: Expr -> [Int]
collect = foldExpr return (++) (++)