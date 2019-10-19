import Prelude hiding (product, length, reverse, zip, drop, (++), and, concat, replicate, (!!), elem)

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial(n-1)

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product(xs)

length :: [a] -> Int 
length [] = 0
length (x:xs) = 1 + length xs

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xs = xs
drop n (_:xs) = drop (n-1) xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys 
xs ++ [] = xs
(x:xs) ++ ys = x  : (xs ++ ys)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = smaller ++ [x] ++ larger
    where smaller = qsort [y | y <- xs, y <= x]
          larger = qsort [z | z <- xs, z > x]

and :: [Bool] -> Bool
and [] = True
and (x:xs) = x && and (xs)