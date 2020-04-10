import System.IO

-- Compute the min size of two lists
minList :: [Int] -> [Int] -> [Int]
minList a b
  | sum a >= sum b = a
  | otherwise      = b

-- Recursive function to find smallest list


-- Computes and prints the smallest k sets
smallestKSets :: [Int] -> Int -> IO ()
smallestKSets list k = putStr "Hello World"
