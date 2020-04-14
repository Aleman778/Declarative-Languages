-- Author: Alexander Mennborg

import System.IO

-- For convenience we define sub list type which is defined by (i, j) tuple.
type SubList = (Int, Int)


-- The size is calculated by taking the sum for a sub list.
size :: Num t => [t] -> SubList -> t
size xs idx = sum (subList xs idx)


-- Sort first by the size of the two sub lists, if they are equal pick the shortest.
order :: (Ord t, Num t) => [t] -> (Int, SubList) -> (Int, SubList) -> Bool
order list (xlen, x) (ylen, y) = 
  let xsize = (size list x)
      ysize = (size list y)
  in if (xsize == ysize)
     then (xlen < ylen)
     else (xsize < ysize)


-- Creates a sublist based on the input indices pair (index j is excusive).
subList :: [t] -> SubList -> [t]
subList xs (i, j) = [xs !! k | k <- [(i - 1)..(j - 1)], 0 <= k, k < (length xs)]


-- Creates all possible sublists with provided max length.
crateSubLists :: Int -> [SubList]
crateSubLists len = [(i, j) | i <- [1..len], 
                              j <- [1..len], 
                              i <= j]


-- Merge sort implementation using order function for determining the order.
-- We are sorting the sublists which are represented by (i, j).
mergeSort :: Ord t => Num t => [t] -> [SubList] -> [SubList]
mergeSort _ []  = []
mergeSort _ [x] = [x]
mergeSort list sublists = 
  let (xs, ys) = split sublists
  in merge (mergeSort list xs) (mergeSort list ys)
  where
    split :: [SubList] -> ([SubList], [SubList])
    split xs = splitAt ((length xs) `div` 2) xs
    merge :: [SubList] -> [SubList] -> [SubList]
    merge x [] = x
    merge [] y = y
    merge (x:xs) (y:ys)
      | order list (length xs, x) (length ys, y) = x : merge xs (y:ys)
      | otherwise                  = y : merge (x:xs) ys 


-- For convenience we define a simple type for storing the spacing between each column.
type Spacing = (Int, Int, Int)


-- Pushes the string right by specified amount.
pushRight :: Int -> String -> String
pushRight x str = 
  let numSpaces = x - (length str) 
  in (replicate numSpaces ' ') ++ str


-- The header row.
headerRow :: [String]
headerRow = ["size", "i", "j", "sublist"]


-- Creates a list of strings for a row given the list and indices.
createRow :: (Num t, Show t) => [t] -> SubList -> [String]
createRow list (i, j) = [show (size list (i, j)), show i, show j, show (subList list (i, j))]


-- Create a matrix containing the results found.
createMatrix :: (Num t, Show t) => [t] -> [SubList] -> [[String]]
createMatrix list [] = [[]]
createMatrix list (x:xs) = (createRow list x) : (createMatrix list xs)


-- Calculates the number of spaces needed between each column.
maxSpacing :: Spacing -> Spacing -> Spacing
maxSpacing (x1, x2, x3) (y1, y2, y3) = (max x1 y1, max x2 y2, max x3 y3)

-- Calculating the spacing, we add 1 to make avoid numbers being to close to each other.
-- Since the last row is right align there is no need to define spacing just use 4 spaces.
calcSpacing :: [[String]] -> Spacing
calcSpacing [[]] = (4, 4, 4) -- The minimum spacing is 4.
calcSpacing [[size, i, j, sublist]] = (length size + 1, length i + 1, length j + 1)
calcSpacing (x:xs) = maxSpacing (calcSpacing [x]) (calcSpacing xs)


-- Convert the selected lists into printable string representation.
formatMatrix :: [[String]] -> Spacing -> String
formatMatrix [[]] _ = ""
formatMatrix [[size, i, j, sublist]] (s1, s2, s3) = 
     pushRight s1 size
  ++ pushRight s2 i
  ++ pushRight s3 j
  ++ pushRight (4 + length sublist) sublist
formatMatrix (x:xs) spacing = (formatMatrix [x] spacing) ++ "\n" ++ (formatMatrix xs spacing)


-- Converts the results from running the program into a nicely formatted string.
formatEntireList :: Show t => [t] -> String
formatEntireList xs = "Entire list: " ++ show xs ++ "\n"


formatResult :: (Num t, Show t) => [t] -> [SubList] -> String
formatResult list sets =
  let matrix = headerRow : (createMatrix list sets)
      spacing = calcSpacing matrix
  in "\n" ++ (formatEntireList list) ++ "\n" ++ (formatMatrix matrix spacing) ++ "\n" 


-- Calculate the smallest k sets
calcSmallestKSets :: (Ord t, Show t, Num t) => [t] -> Int -> String
calcSmallestKSets list k = 
  let sublists = crateSubLists (length list)
  in formatResult list (take k (mergeSort list sublists))


-- Entry point performs computation and prints results.
smallestKSets :: (Ord t, Show t, Num t) => [t] -> Int -> IO ()
smallestKSets list k
  | length list > 0 = putStr (calcSmallestKSets list k)
  | otherwise       = error "Smallest K sets does not accept an empty list."


runTestCases :: IO ()
runTestCases = putStr (
     "Test case 1:\n" ++ separator ++ "\n" ++ testCase1 
  ++ "Test case 2:\n" ++ separator ++ "\n" ++ testCase2 
  ++ "Test case 3:\n" ++ separator ++ "\n" ++ testCase3)
  where
    separator :: String
    separator = replicate 50 '-' 
    testCase1 :: String
    testCase1 = calcSmallestKSets [x*(-1)^x | x <- [1..100]] 15
    testCase2 :: String
    testCase2 = calcSmallestKSets [24, -11, -34, 42, -24, 7, -19, 21] 6
    testCase3 :: String
    testCase3 = calcSmallestKSets [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3] 8
