module Assignment1 where
import Prelude

-- problem 1

-- pattern matches for base case and then recursively call itself for further cases
randGen :: Int -> Int -> Int -> Int -> Int -> Int
randGen 1 x0 a b c = (a*x0 + b) `mod` c
randGen idx x0 a b c = (a * randGen (idx - 1) x0 a b c + b) `mod` c

-- problem 2

-- pattern matches for empty list, ignores spaces when encountered and in other cases finds the first word 
-- puts it in the beginning of the list and then calls itself on the rest of the string
split :: String -> [String]
split [] = []
split (x:xs) | x == ' ' = split xs
             | otherwise = word : split rest
                where (word, rest) = break (== ' ') (x:xs)

-- problem 3

-- helper function that sorts a list (taken from slides)
sort :: [Int] -> [Int]
sort [] = []
sort [x] = [x]
sort (x:xs) = small ++ [x] ++ large
    where small = sort [y | y <- xs, y <= x]
          large = sort [y | y <- xs, y > x]

-- pattern matches for empty list 
-- if length is odd then rescursively calls itself on the tail of the sorted list and adds the largest element to the end of the list
wave :: [Int] -> [Int]
wave [] = []
wave xs  = if (length xs `mod` 2 == 1) then ((wave ys) ++ [y]) else (y: (wave ys))
    where y = head (sort xs)
          ys = tail (sort xs)


-- problem 4

-- generates a list of catalan numbers using recursion and then picks the nth catalan number
numBST :: Int -> Integer
numBST n = (catalans !! n) `mod` (10^9 + 7)
    where catalans = 1 : 1 : [ number | x <- [2..n], let number = sum([((catalans !! (y)) * (catalans !! (x-y-1))) |  y <- [0..(x-1)]])]
    
   
-- problem 5
-- Find the occurence of digit in a number
occ :: Int -> Int -> Int
occ 0 _ = 0
occ x y = (if  (x `mod` 10 == y) then 1 else 0) + occ (quot x 10) y

-- compares the sum of the occurrences of 1 and 2 in each number in a list and returns true if 1s are greater otherwise false
isMartian :: [Int] -> Bool
isMartian [] = False
isMartian xs = sum([occ x 1 | x <- xs]) > sum([occ x 2  | x <- xs])


-- problem 6
-- Generates pairs of adjacent elements
pairs :: [Int] -> [(Int, Int)]
pairs xs = zip xs (tail xs)

-- Checks if the list is sorted in the ascending order
isAscSorted :: [Int] -> Bool
isAscSorted xs = and [x <= y | (x, y) <- pairs xs]

-- Checks if the list is sorted in the descending order
isDescSorted :: [Int] -> Bool
isDescSorted xs = and [x >= y | (x,y) <- pairs xs]

-- Divides the list into two lists of even and odd numbers and checks if they are sorted in ascending and descending order respectively
isTwinPaired :: [Int] -> Bool
isTwinPaired xs = isAscSorted [x | x <- xs, even x] && isDescSorted [x | x <- xs, odd x]

-- problem 7
-- Checking Short L condition
checkShortL :: Int -> Int -> Int -> Int -> Bool
checkShortL a b x y = abs(a-x) == 2 && abs(b-y) == 1

--Checking Long L condition
checkLongL :: Int -> Int -> Int -> Int -> Bool
checkLongL a b x y = (abs(a-x) == 1 && abs (b-y) == 2)

-- Checks if either of the L conditions are true or not (returns true if L is not being occupied)
checkL :: Int -> Int -> Int -> Int -> Bool
checkL a b x y =  not ((checkShortL a b x y) || (checkLongL a b x y))

-- Checks if the x, y coordinates are occupied or not 
checkCoordi a b x y = (a /= x) && (b /= y)

-- Checks if the x, y are not on the diagonal
checkDiagonal a b x y = abs(a-x) /= abs(b-y)

-- Checks whether knight is allowed in this location
safePosition :: Int -> Int -> Int -> [Int] -> Bool
safePosition _ _ _ [] = True
safePosition x y r (c:cs) = (checkCoordi x y r c) && (checkDiagonal x y r c) && (checkL x y r c) && (safePosition x y (r+1) cs)
 
-- recurs for each row and finds columns where it can add a new knight for that row.
chessHelper :: Int -> Int -> [Int] -> Int
chessHelper n row cols | n == row = 1              
                       | otherwise = sum [(chessHelper n (row+1) (cols ++ [col])) | col <- [0..n-1], (safePosition row col 0 cols) ]

-- Calls helper function after fixing a knight in a particular column in row 0
chess :: Int -> Int
chess n = sum [(chessHelper n 1 [col]) | col <- [0..(n-1)]]