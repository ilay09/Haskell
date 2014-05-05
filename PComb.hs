module PComb (
	Parser,
	parse,
	getChar,
	(<|>),
	satisfy,
	alpha, digit, upper, lower, space,
	char,
	string,
	many,
	oneOrMore
) where

import Prelude hiding (getChar)
import Date.Char


{- 1. Парсер может ничего не распорсить
 2. Парсер может распорсить несколько вариантов
-}
newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

{- 
Выдерживает 1 символ из строки - базовый комбинатор
-}
getChar :: Parser Char
getChar = P $ \cs -> case cs of
	(x:xs) -> [(x,xs)]
	[] -> []
	

{-
3-й парсер вызывает результат или 1 или 2 парсера
-}

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> case parse p1 cs of
	[] -> parse p2 cs
	ps -> ps
	
{-
Парсер, который возвращает значения не разбирая строку
-}

instance Monad Parser where
 p1 >>= fp2 = P $ \cs -> do (a, cs') <- parse p1 cs 
							parse (fp2 a) cs'
return x = P $ \cs -> [(x,cs)]
fail _ = P $ \_ -> []			

{-
Применяем к парсеру функцию
-}
instance Functor Parser where
	fmap f p = p >>= \x -> return (f x)
	
{- Парсер satisfy(условие) -}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
	c <- getChar
	if p c then return c else fail "Did not satisfy boolean"
	
alpha, digit, upper, lower, space :: Parser Char
alpha = satisfy isAlpha
digit = satisfy isDigit
upper = satisfy isUpper
lower = satisfy isLower
space = satisfy isSpace

{-
Возвращает символ тогда, когда символ совпадет с желаемым
-}

char :: Char -> Parser Char
char = satisfy . (==)	
	
{-
Комбинатор, который сразу распознает строку. mapM - map монадический
-}	
	
string :: String -> Parser String
string = mapM char

{-
Распознает 0 или больше символов
-}	
	
many :: Parser a -> Parser [a]
many p = oneOrMore p <|> (return [])

{-
Распознает как минимум 1 
-}

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = do x <- p 
				xs <- many p 
				return (x:xs)
				
				
				
module Main where

int :: Parser Int
int = do
	s <- string "-" <|> return []
	d <- oneOrMore digit 
	return (read (s ++ d) :: Int)
	
psum :: Parser Int
psum = do
	i1 <- int
	many space
	char '+'
	many space
	i2 <- int
	return $ i1 + i2 
	
psub :: Parser Int
psub = do
	i1 <- int
	many space
	char '-'
	many space
	i2 <- int
	return $ i1 - i2 	
	
multi :: Parser Int
multi = do
	i1 <- int
	many space
	char '*'
	many space
	i2 <- int
	return $ i1 * i2	
	
div :: Parser Int
div = do
	i1 <- int
	many space
	char '/'
	many space
	i2 <- int
	return $ i1 / i2	
	
degree :: Parser Int
degree = do
	i1 <- int
	many space
	char '^'
	many space
	i2 <- int
	return $ i1 ** i2

	
arith :: Parser Int
arith = psum <|> psub <|> multi <|> div <|> degree



	
main = do
	putStrLn "Enter integer number:"
	l <- getLine
	case parse arith l of
		[] -> putStrLn "Incorrect integer"
		((i,_):_) -> putStr "Parsed: " >> putStrLn (show arith)

