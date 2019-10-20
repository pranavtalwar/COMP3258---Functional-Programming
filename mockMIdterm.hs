--q1 a

dup :: [a] -> [a]
dup [] = []
dup (x:[]) = [x]
dup (x:y:xs) = x: y : y : (dup xs) 

--q1 b 
dup' :: [a] -> [a]
dup' xs = concat [replicate (1 + (xi `mod` 2)) x | (x,xi) <- zip xs [0..(length xs -1)]]

--q2 a 
hour :: [Int] -> Int 
hour xs = hour' 0 xs

hour' :: Int -> [Int] -> Int 
hour' mins [] = ((mins `div` 60) `mod` 12) + 1
hour' mins (x:xs)
    | x > 0 = hour' (mins+x) xs
    | otherwise = hour' mins xs

--q2 b
hour'' :: [Int] -> Int 
hour'' xs = (sum ([x | x <- xs, x > 0]) `div` 60) `mod` 12 + 1

--q2 c
hour''' :: [Int] -> Int
hour''' xs = ((foldr (+) 0 (filter (>0) xs)) `div` 60) `mod` 12 + 1

--q3 a
data Expr = X | Const Int | Neg Expr | Add Expr Expr | Mul Expr Expr deriving (Show)

eval :: Expr -> Int -> Int 
eval X num = num
eval (Const a) num = a
eval (Neg a) num = -(eval a num)
eval (Add expr1 expr2) num = (eval expr1 num) + (eval expr2 num)
eval (Mul expr1 expr2) num = (eval expr1 num) * (eval expr2 num)

--q3 b
rpn :: Expr -> [String]
rpn X = ["X"]
rpn (Const c) = [show c]
rpn (Neg expr) = rpn expr ++ ["-"]
rpn (Add expr1 expr2) = (rpn expr1) ++ (rpn expr2) ++ ["+"]
rpn (Mul expr1 expr2) = (rpn expr1) ++ (rpn expr2) ++ ["*"]

