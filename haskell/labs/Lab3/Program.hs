-- Modified by: Alexander Mennborg


module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program [Statement.T] deriving Show

program :: Parser T
program = iter Statement.parse >-> Program

shw :: T -> String
shw (Program stmts) = foldr (\x y -> Statement.toString x ++ "\n" ++ y) "" stmts

exec :: T -> [Integer] -> [Integer]
exec (Program stmts) input = Statement.exec stmts  Dictionary.empty input

instance Parse T where
  parse = program
  toString = shw
             
