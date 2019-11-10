import Prelude hiding (Nothing, Just, Maybe)

-- infixr 5 +++

data Maybe a = Just a | Nothing deriving Show

instance Functor Maybe where
    fmap f (Just a) = Just (f a)
    fmap f Nothing = Nothing

instance Applicative Maybe where 
    pure = Just 
    Just f <*> j = f <$> j
    Nothing <*> j = Nothing


data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving (Show)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r) 

instance Applicative Tree where
    pure = Leaf 
    Leaf f <*> t = f <$> t