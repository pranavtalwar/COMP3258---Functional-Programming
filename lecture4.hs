import Prelude hiding (abs, signum, (&&), (||), head, tail)

abs :: (Ord a, Num a) => a -> a
abs x = if x >= 0 then x else -x

abs' :: (Ord a, Num a) => a -> a
abs' x | x >=0 = x
       | otherwise = -x

signum :: (Ord a, Num a) => a -> Int
signum x = if x > 0 then 1 else
            if x == 0 then 0 else -1

signum' :: (Ord a, Num a) => a -> Int
signum' x | x > 0 = 1
          | x == 0 = 0
          | otherwise = -1

-- (&&) :: Bool -> Bool -> Bool
-- True && b = b
-- False && _ = False

head :: [a] -> a
head [] = error "empty list"
head (x:_) = x

tail :: [a] -> [a]
tail [] = error "empty list"
tail (_:xs) = xs

odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..(n-1)]

safetail :: [a] -> [a]
safetail xs = if null xs then [] else tail xs

safetail' :: [a] -> [a]
safetail' xs | null xs = []
             | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' (x:xs) = xs

(||) :: Bool -> Bool -> Bool
True || _ = True
_ || True = True
False || b = b

-- (&&) :: Bool -> Bool -> Bool
-- (&&) x y = if x /= True then False else
--             if y /= True then False else True

(&&) :: Bool -> Bool -> Bool
(&&) x y = if x == True then y else False
