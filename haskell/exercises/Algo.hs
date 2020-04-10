  
-- Merge Sort Implementation

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = 
  let (xs, ys) = split list 
  in merge (mergeSort xs) (mergeSort ys)
  where
    split xs = splitAt ((length xs) `div` 2) xs

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge [] (y:ys) = [y]
merge (x:xs) [] = [x]
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
