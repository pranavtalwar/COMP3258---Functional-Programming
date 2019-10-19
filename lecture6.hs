import Prelude hiding (map, filter, foldr, length, reverse, (++), (.), all, any, takeWhile,dropWhile)
    
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: (Ord a, Num a) => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions n xs = [xi | (x, xi)<- zip xs [0..(length xs -1)], x  == n]

count :: Char -> String -> Int
count c [] = 0 
count c (x:xs) = if x == c then 1 + (count c xs) else count c xs

scalar :: [Int] ->[Int] -> Int
scalar xs ys = sum ([x*y | (x,xi) <- zip xs [0..(length xs -1)], (y,yi) <- zip ys [0..(length ys-1)], xi == yi])

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x<- xs]

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = (f x) : (map' f xs)

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [x | x  <- xs, f x]

filter' :: (a-> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x : (filter' f xs) else (filter' f xs)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v [] = v 
foldr f v (x:xs) = x `f` foldr f v xs

length :: [a] -> Int
length = foldr (\x r-> 1 + r) 0

reverse :: [a] -> [a]
reverse = foldr (\x r -> r ++ [x]) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (\x xs -> x : xs) ys xs

(.) :: (b->c) -> (a->b) -> (a->c)
f . g = \x -> f (g x)

all :: (a -> Bool) -> [a] -> Bool
all p xs = and [p x | x <- xs]

any :: (a -> Bool) -> [a] -> Bool
any p xs = or [p x | x <- xs]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) = if p x then x: (takeWhile p xs) else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = [] 
dropWhile p (x:xs) = if p x then (dropWhile p xs) else x:xs






