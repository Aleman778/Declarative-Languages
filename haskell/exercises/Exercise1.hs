
-- Chapter 3 --

-- Exercise 3.7 on page 37

threeDifferent1 :: Int -> Int -> Int -> Bool
threeDifferent1 a b c
  | a /= b && b /= c && a /= c = True
  | otherwise = False

threeDifferent2 :: Int -> Int -> Int -> Bool
threeDifferent2 a b c = 
  if a /= b 
  then if b /= c 
       then if a /= c 
            then True 
            else False 
       else False
  else False

-- threeDifferent 3 4 3 returns False since a /= c condition fails


-- Exercise 3.8 on page 37

twoEqual :: Int -> Int -> Bool
twoEqual a b = if a == b then True else False

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = twoEqual a b && b == c

fourEqual1 :: Int -> Int -> Int -> Int -> Bool
fourEqual1 a b c d = threeEqual a b c && c == d

  
fourEqual2 :: Int -> Int -> Int -> Int -> Bool
fourEqual2 a b c d = 
  if a == b 
  then if b == c
       then if c == d
            then True
            else False
       else False
  else False


-- Exercise 3.17 on page 46

smallerRoot, largerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c = let x = (b ** 2 - 4 * a * c) in
  if x > 0
  then ((-b) - sqrt x) / (2 * a)
  else 0

largerRoot a b c = let x = (b ** 2 - 4 * a * c) in
  if x > 0
  then ((-b) + sqrt x) / (2 * a)
  else 0

-- Lets combine smallerRoot and largerRoot into one, and support complex numbers aswell

findRoots :: Float -> Float -> Float -> ((Float, Float), (Float, Float))
findRoots a b c =
  let lhs = ((-b)/(2*a))
      sqr = b**2 - 4*a*c
  in if sqr >= 0
     then let rhs = (sqrt sqr)/(2*a)
          in ((lhs - rhs, 0), (lhs + rhs, 0))
     else let rhs_complex = (sqrt (-sqr))/(2*a)
          in ((lhs, -rhs_complex), (lhs, rhs_complex))
     


-- Chapter 4 --  

-- Exercise 4.7 on page 64
recMul :: Int -> Int -> Int
recMul 0 b = 0
recMul a b = b + (a - 1) `recMul` b

{-
Questions:
For what numbers of a should the function be defined? Any integer a > 0.

What is the base case? For a == 0 we get 0 since 0 * b = 0. Since b is constant it is
not neccessary to specify a base case where b == 0.
-}

-- Exercise 4.8 on page 64

intSqrt :: Int -> Int
intSqrt a = implSqrt a 1
  where
    implSqrt 0 1 = 0
    implSqrt a b = if b*b > a
      then 0
      else 1 + implSqrt a (b + 1)


-- Exercise 4.9 on page 64

-- Given function f
f 0 = 0
f 1 = 44
f 2 = 17
f _ = 0

-- Using list comprehension and recursion (linear search)
maxFList :: Int -> Int
maxFList n = maxElem [f x | x <- [0..n]]
  where
    maxElem [] = 0
    maxElem list = max (head list) (maxElem (tail list))

-- Using only recursion (linear search)
maxFLinRec :: Int -> Int
maxFLinRec 0 = f 0
maxFLinRec n = max (f n) (maxFLinRec (n - 1))


-- Exercise 4.14 on page 67


powerOfTwo :: Int -> Int
powerOfTwo 0 = 1
powerOfTwo 1 = 2
powerOfTwo n = let m = n `div` 2 in 
  if (isEven n)
     then 
       (powerOfTwo m)^2
     else 
       ((powerOfTwo m)^2)*2
  where
    isEven a = a `mod` 2 == 0
