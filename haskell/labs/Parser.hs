-- Modified by: Alexander Mennborg


module Parser(module CoreParser, T, digit, digitVal, chars, letter, err,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #- 

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]  
iter m = m # iter m >-> cons ! return [] 

cons(a, b) = a:b

token :: Parser a -> Parser a
token m = m #- spaces

word :: Parser String
word = token (letter # iter letter >-> cons)


accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char 
digit = char ? isDigit 

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

-- Assignment Part 1 --

letter :: Parser Char
letter = char ? isAlpha

spaces :: Parser String
spaces = iter (char ? (== ' '))

chars :: Int -> Parser String
chars n
  | n > 0 = char # chars (n - 1) >-> cons
  | otherwise = return []

require :: String -> Parser String
require w = (accept w) ! (err ("expected `" ++ w ++ "`"))

(-#) :: Parser a -> Parser b -> Parser b
m -# n = m # n >->snd

(#-) :: Parser a -> Parser b -> Parser a
m #- n = m # n >-> fst