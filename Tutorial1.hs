import Prelude hiding (last, init, curry, uncurry, signum)

double :: Num a => a -> a 
double x = x + x 

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial :: Int -> Int
factorial x = product [1..x]

factorial2 :: Int -> Int 
factorial2 0 = 1
factorial2 x = x * factorial(x-1)

average :: [Int] -> Int
average [] = 0
average xs = sum xs `div` length xs

-- in haskell variable and function names start with a small letter
n = a `div` length xs
    where a = 10
          xs = [1,2,3,4,5]

-- defining the library function last using different approaches

-- approach 1 using recursion
last :: [a] -> a
last [] = error "empty list"
last (x:[]) = x
last (x:xs) = last xs

-- approach 2 using reverse and returning head
last2 :: [a] -> a
last2 xs = head(reverse xs)

-- approach 3 using list addressing 
last3 :: [a] -> a
last3 [] = error "empty list"
last3 xs = xs !! (length xs - 1)

-- defining the library function init using different approaches

-- approach 1 using reverse and tail
init :: [a] -> [a]
init xs = reverse (tail ( reverse xs))

-- approach 2 using recursion
init2 :: [a] -> [a]
init2 [] = error "empty list"
init2 (x:[]) = []
init2 (x:xs) = [x] ++ init2 xs

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y 

-- signum using if else
signum x = if x<0 then -1 else 
            if x == 0 then 0 else 1

-- signum using guards
signum2 x | x < 0 = -1
          | x == 0 = 0
          | otherwise = 1


-- implementation of tail
tl :: [a] -> [a]
tl [] = error "empty list"
tl (x:xs) = xs


