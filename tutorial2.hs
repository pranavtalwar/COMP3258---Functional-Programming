import Prelude hiding (concat, replicate, (!!), elem)

-- q1 concatenate a list
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ concat(xss)

-- replicate an element n times
replicate :: Int -> a -> [a]
replicate n x
    | n < 0 = error "must be positive"
    | n == 0 = []
    | otherwise = x : replicate (n-1) x

-- access the nth element of the list
(!!):: [a] -> Int -> a
[] !! _ = error "index too large"
(x:xs) !! n = if n == 0 then x else (xs !! (n-1))

-- check if an element is in a list
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs) = if n == x then True else elem n xs 

-- q2 fibonacci sequence
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


-- q3 1. recursive function that doubles everything in a list
listDouble :: [Int] -> [Int]
listDouble [] = []
listDouble (x:xs) = x*2 : listDouble xs

-- q3 1. using map and lambda that doubles everything in a list
listDouble' :: [Int] -> [Int]
listDouble' xs = map (\x -> x*2) xs

-- q4 
zipSum :: [Int] -> [Int] -> [Int]
zipSum [] _ = []
zipSum _ [] = []
zipSum (x:xs) (y:ys) = (x+y) : (zipSum xs ys)

-- implementation using zipWith
zipSum' :: [Int] -> [Int] -> [Int]
zipSum' = zipWith (+)

-- q5 
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : (merge xs (y:ys)) else y : (merge (x:xs) ys)

-- q6
msort :: Ord a => [a] -> [a]
msort [x] = [x]
msort xs = merge (msort left) (msort right)
        where (left, right) = splitAt ((length xs) `div` 2) xs

-- q7
pythagoreans :: Int -> [(Int, Int, Int)]
pythagoreans n = [(x,y,z) | x <- [1..n], y <-[1..n], z <- [1..n], x^2 + y^2 == z^2]

-- q8
perfect :: Int -> [Int]
perfect n = [x | x <- [1..n], isPerfect x]
    where isPerfect y = sum([z | z <- [1..(y-1)], y `mod` z == 0]) == y
