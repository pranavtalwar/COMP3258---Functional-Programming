import Data.Char 
import Data.List
import Control.Monad
import Control.Applicative

data Box = Box Int Int Int [Box] deriving (Eq)
 
numBox :: Box -> Int 
numBox (Box a b c []) = 1
numBox (Box a b c xs) = 1 + sum([numBox x | x <- xs])

instance Show Box where
  show (Box a b c []) = "(length:" ++ (show a) ++ ", width:" ++ (show b) ++ ", height:" ++ (show c) ++ ")"
  show (Box a b c xs) = "(length:" ++ (show a) ++ ", width:" ++ (show b) ++ ", height:" ++ (show c) ++ " " ++ intercalate ", " [show x | x <- xs] ++ ")"

------------------------------------------------------------
-------------------- basic monadic parser --------------------
newtype Parser a = Parser { runParse :: String -> [(a,String)] }

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]
   
bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> runParse (f a) s') $ runParse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a,s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=)  = bind
  
instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> runParse p s ++ runParse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case runParse p s of
    []     -> runParse q s
    res    -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then unit c
  else failure
  
natural :: Parser Int
natural = read <$> some (satisfy isDigit)

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

spaces :: Parser String
spaces = many $ oneOf " \n\r\t"

------------------------------------------------------------
------------------------------------------------------------

boxParser :: Parser Box
boxParser = do
    string "<box"
    spaces
    string "length="
    length <- natural
    spaces
    string "width="
    width <- natural
    spaces
    string "height="
    height <- natural
    spaces
    string ">"
    spaces
    boxList <- boxlistParser
    spaces
    string "</box>" 
    return (Box length width height boxList)

boxlistParser :: Parser [Box]
boxlistParser = option boxes space
    where
        boxes = do
            box <- boxParser
            spaces
            boxlist <- boxlistParser
            return (box:boxlist)
        space = do
            string ""
            return []

parse :: String -> Box
parse xs = fst (head (runParse boxParser xs))

isValid :: String -> Bool
isValid xs = if (runParse boxParser xs) == [] then False else (lengthChecker box) && (widthChecker box) 
            where box = parse xs

lengthChecker :: Box -> Bool
lengthChecker (Box l w h [])= True
lengthChecker (Box l w h xs) = (l > sum[il | (Box il iw ih ixs) <- xs]) && (and [lengthChecker ib | ib <- xs])

widthChecker :: Box -> Bool
widthChecker (Box l w h []) = True
widthChecker (Box l w h xs) = (w > maximum [iw | (Box il iw ih ixs) <- xs]) && (and [widthChecker ib | ib <- xs])

heightChecker :: Box -> Bool
heightChecker (Box l w h []) = True 
heightChecker (Box l w h xs) = (h > maximum [ih | (Box il iw ih ixs) <- xs]) && (and [heightChecker ib | ib <- xs])

