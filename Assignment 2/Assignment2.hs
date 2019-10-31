import Data.List
data Box = Box Int Int Int [Box] deriving (Eq)

-- problem 1
numBox :: Box -> Int 
numBox (Box a b c []) = 1
numBox (Box a b c xs) = 1 + sum([numBox x | x <- xs])

-- problem 2
instance Show Box where 
    show (Box a b c []) = "(length:" ++ (show a) ++ ", width:" ++ (show b) ++ ", height:" ++ (show c) ++ ")"
    show (Box a b c xs) = "(length:" ++ (show a) ++ ", width:" ++ (show b) ++ ", height:" ++ (show c) ++ " " ++ intercalate ", " [show x | x <- xs] ++ ")"