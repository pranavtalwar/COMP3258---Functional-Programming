import Prelude hiding (return)
import Data.Char
import Control.Monad
import Control.Applicative hiding (many)

data Parser a = P (String -> [(a, String)])

item :: Parser Char 
item = P (\inp -> case inp of 
                    [] -> []
                    (x:xs) -> [(x,xs)])

failure :: Parser a 
failure = P (\inp -> [])

(+++) :: Parser a -> Parser a -> Parser a 
p +++ q = P (\inp -> case parse p inp of 
                        [] -> parse q inp
                        [r] -> [r])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where 
    fmap f (P p) = P (\inp -> fmap (\(c,xs) -> (f c, xs)) (p inp))

instance Applicative Parser where 
    pure = return 
    (<*>) = ap

instance Alternative Parser where
    empty = failure
    (<|>) = (+++)

instance Monad Parser where 
    return v = P (\inp -> [(v, inp)])
    p >>= f = P (\inp -> case parse p inp of
                        [] -> []
                        [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where 
    mzero = P(\inp -> [])
    p `mplus` q = P (\inp -> case parse p inp of
                        [] -> parse q inp
                        [r] -> [r])