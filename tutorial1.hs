import Prelude hiding (last, init, curry, uncurry, signum)

-- Q1. function names and variable names start with lowercase letter
n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5]

-- Q2. implementing last library funcion using other functions
last :: [a] -> a
last [] = error "empty list"
last xs = head (reverse xs)

-- implementing recursion version
last' :: [a] -> a 
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

-- Q3 implementing init librayr function using other library functions
init :: [a] -> [a]
init [] = error "empty list"
init xs = reverse (drop 1 (reverse xs))

-- implementing recursion version
init' :: [a] -> [a]
init' [] = error "empty list"
init' (x:[]) = []
init' (x:xs) = x: init' xs

-- Q4 -- implementing curry and uncurry
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x,y) = f x y 

-- implementing signum using if else and guards

signum :: Int -> Int
signum x = if x < 0 then -1 else 
            if x > 0 then 1 else 0

signum' :: Int -> Int
signum' x | x == 0 = 0
         | x > 0 = 1
         | otherwise = -1 

-- implementing tail using pattern matching
tl :: [a] -> [a]
tl [] = error "empty list"
tl (x:xs) = xs


-- Q5 (pattern matching)
first :: (a,b) -> a
first (a, _) = a

isZero :: Int -> Bool
isZero 0 = True
isZero _ = False

-- Q6 
isleapyear :: Int -> Bool
isleapyear year = if year `mod` 4 /= 0 then False else
                    if year `mod` 100 /= 0 then True else
                        if year `mod` 400 == 0 then True else False

-- Q7
safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' [] = []
safetail' xs = tail xs

safetail'' :: [a] -> [a]
safetail'' xs | null xs = []
              | otherwise = tail xs

-- lambda example
add :: Int -> Int -> Int 
add = \x -> (\y -> x + y)






