

-- Chapter 9 starting on page 152


-- Exercise 9.2 on page 159

length' :: [a] -> Int
length' xs = sum (map (\x -> 1) xs) 


-- Exercise 9.4 on page 160

addOne :: Int -> Int
addOne x = x + 1

-- This would add two to every element in `ns` since we 
-- calculate addOne to each element twice.
addTwo :: [Int] -> [Int]
addTwo ns = map addOne (map addOne ns)

-- In the general case this would be the same as first
-- applying the function g on ns and then f on the previous result
mapMap :: (b -> c) -> (a -> b) -> [a] -> [c]
mapMap f g ns = map f (map g ns)

-- This can be done using only one map since we what we are
-- doing in the previous exercise is applying f on g's output
-- that can be expressed using the dot operator as f.g
mapMap2 :: (b -> c) -> (a -> b) -> [a] -> [c]
mapMap2 f g ns = map (f.g) ns


-- Exercise 9.6 on page 160
square :: [Int] -> [Int]
square ns = map (\x -> x*x) ns

squareLC :: [Int] -> [Int]
squareLC ns = [x*x | x <- ns]

sumSquares :: [Int] -> Int
sumSquares ns = foldr (\x y -> x*x + y) 0 ns

sumSquaresLC :: [Int] -> Int
sumSquaresLC ns = sum [x*x | x <- ns]

checkPositive :: [Int] -> Bool
checkPositive ns = foldr positive True ns
  where
    positive :: Int -> Bool -> Bool
    positive x b = if x >= 0 then (True && b) else False

checkPositiveLC :: [Int] -> Bool
checkPositiveLC ns = and [if x >= 0 then True else False | x <- ns]


-- Exercise 9.7 on page 160
maxFunc :: Ord a => (Int -> a) -> Int -> a
maxFunc f n = foldr max (f 0) [f x | x <- [0..n]]


eqFunc :: Ord a => (Int -> a) -> Int -> Bool
eqFunc f n = foldr eqF True [(f x, f y) | x <- [0..n], y <- [0..n], x /= y]
  where
    eqF :: Ord a => (a, a) -> Bool -> Bool
    eqF (x, y) b = (x == y) && b


gtZeroFunc :: (Num a, Ord a) => (Int -> a) -> Int -> Bool
gtZeroFunc f n = foldr gtZeroF True [f x | x <- [0..n]]
  where
    gtZeroF :: (Num a, Ord a) => a -> Bool -> Bool
    gtZeroF x b = (x > 0) && b
  

-- Problem for later (implement map using only foldr and lambda-expressions)
-- mapTest :: (a -> b) -> [a] -> [b]
-- mapTest f (x:xs) = foldr (\x -> y -> (f x)) xs
