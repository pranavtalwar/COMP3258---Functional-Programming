import Prelude hiding (flip)

data Answer = Yes | No | Unknown deriving (Show)

answers = [Yes, No, Unknown]

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown

data Shape = Circle Float | Rect Float Float deriving (Show)

square :: Float -> Shape 
square n = Rect n n 

area :: Shape -> Float 
area (Circle r) = (*) 3.14 ((^) r 2)
area (Rect x y) = (*) x y 

data Nat = Zero | Succ Nat deriving (Show)

nat2Int :: Nat -> Int 
nat2Int Zero = 0
nat2Int (Succ n) = 1 + (nat2Int n) 

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat n = Succ(int2Nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ(add m n)

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

size :: Expr -> Int 
size (Val n) = 1
size (Add expr1 expr2) = size (expr1) + size(expr2)
size (Mul expr1 expr2) = size (expr1) + size(expr2)

eval :: Expr -> Int 
eval (Val n) = n
eval (Add expr1 expr2) = eval(expr1) + eval(expr2)
eval (Mul expr1 expr2) = eval(expr1) * eval(expr2)

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

t :: Tree Int 
t  = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- for binary tree
occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf a) = x == a
occurs' x (Node l n r) = x == n || occurs' x l || occurs' x r

 -- for search tree
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf a) = x == a
occurs x (Node l n r)
    | x == n = True
    | x > n = occurs x r
    | otherwise = occurs x l

flatten :: Tree a -> [a]
flatten (Leaf a) = [a]
flatten (Node l n r) = nl ++ [n] ++ nr
    where nl = flatten l
          nr = flatten r

mul :: Nat -> Nat -> Nat
mul Zero n = Zero
mul (Succ m) n = add (mul m n) n

height :: Tree a -> Int 
height (Leaf a) = 0
height (Node l n r) = 1 + max (height l) (height r)

isComplete :: Tree a -> Bool
isComplete (Leaf a) = True 
isComplete (Node l n r) = (height l == height r) && (isComplete l) && (isComplete r)


