-- Modified by: Alexander Mennborg


module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (repeat, read, return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    Skip |
    Body [Statement] |
    If Expr.T Statement Statement |
    While Expr.T Statement |
    Repeat Statement Expr.T Bool | -- Added bool at the end to signal if the first iteration has been executed.
    Read String |
    Write Expr.T 
    deriving Show

-- Exercise Part 2 b. --

assignment, skip, body, ifStatement, while, read, write, statement :: Parser Statement

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

skip = accept "skip" -# require ";" >-> (\ _ -> Skip)

body = accept "begin" -# (iter statement) #- accept "end" >-> Body
 
ifStatement = accept "if" -# Expr.parse # require "then" -# 
  statement # require "else" -# statement >-> buildIfStatement
buildIfStatement ((c, t), e) = If c t e

while = accept "while" -# Expr.parse # require "do" -# statement >-> (\(c, s) -> While c s)

repeat = accept "repeat" -# statement # require "until" -# Expr.parse #- require ";" >-> (\(s, c) -> Repeat s c True)

read = accept "read" -# word #- require ";" >-> Read

write = accept "write" -# Expr.parse #- require ";" >-> Write

statement = assignment ! skip ! body ! ifStatement ! while ! repeat ! read ! read ! write

-- Exercise Part 2 d. Statement interpreter 
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

-- Execute when there are no more expressions
exec [] _ _ = []

-- Executes assignment statements
exec (Assignment var val: stmts) dict input = 
  exec stmts (Dictionary.insert (var, Expr.value val dict) dict) input

-- Executes skip which does nothing
exec (Skip: stmts) dict input = exec stmts dict input --error (shw 0 (stmts !! 0)) 

-- Executes body (begin ... end) statements
exec (Body bodyStmts: stmts) dict input = 
  exec (bodyStmts ++ stmts) dict input

-- Executes if statements
exec (If cond thenStmts elseStmts: stmts) dict input = 
  if (Expr.value cond dict)>0 
  then exec (thenStmts: stmts) dict input
  else exec (elseStmts: stmts) dict input

-- Executes while statement
exec (While cond doStmts: stmts) dict input =
  if (Expr.value cond dict) > 0
  then exec (doStmts : (While cond doStmts) : stmts) dict input
  else exec stmts dict input

-- Executes while statement
exec (Repeat doStmts cond doIter: stmts) dict input =
  if doIter || (Expr.value cond dict) <= 0
  then exec (doStmts : (Repeat doStmts cond False) : stmts) dict input
  else exec stmts dict input
    
-- Executes read statements
exec (Read var: stmts) dict input = case input of
  []     -> error "there is nothing left to read from"
  (x:xs) -> exec stmts (Dictionary.insert (var, x) dict) xs

-- Executes write statements
exec (Write expr: stmts) dict input =
  (Expr.value expr dict) : (exec stmts dict input)


-- To string intermediate function
shw :: Int -> T -> String 
shw indent (Assignment n i) = 
  (replicate indent ' ') ++ n ++ " := " ++ (Expr.toString i) ++ ";"

shw indent (Skip) = 
  (replicate indent ' ') ++ "skip;"

shw indent (Body stmts) = 
  (replicate indent ' ') ++ "begin\n" ++ foldr (\x s -> (shw (indent + 2) x) ++ "\n" ++ s) "" stmts ++
  (replicate indent ' ') ++ "end"

shw indent (If c t e) = 
  (replicate indent ' ') ++ "if " ++ (Expr.toString c) ++ " then\n" ++ (shw (indent + 2) t) ++ "\n" ++ 
  (replicate indent ' ') ++ "else\n" ++ (shw (indent + 2) e)

shw indent (While c s) = 
  (replicate indent ' ') ++ "while " ++ (Expr.toString c) ++ " do\n" ++ (shw (indent + 2) s)

shw indent (Repeat s c _) = 
  (replicate indent ' ') ++ "repeat\n" ++ (shw (indent + 2) s) ++ "\n" ++
  (replicate indent ' ') ++ "until " ++ (Expr.toString c) ++ ";"

shw indent (Read s) = 
  (replicate indent ' ') ++ "read " ++ s ++ ";"

shw indent (Write e) = 
  (replicate indent ' ') ++ "write " ++ (Expr.toString e) ++ ";"

-- Exercise 2 c. -- 

instance Parse Statement where
  parse = statement
  toString = shw 0
