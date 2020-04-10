
-- Chapter 5

-- Exercise 5.2 on page 77

orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) 
  | b <= a && a <= c = (b, a, c)
  | c <= a && a <= b = (c, a, b)
  | a <= c && c <= b = (a, c, b)
  | c <= a && a <= b = (c, a, b)
  | c <= b && b <= a = (c, b, a)
  | otherwise        = (a, b, c)


-- Exercise 5.10 on page 82
    
divisor :: Int -> [Int]
divisor n = [x | x <- [1..n], n `mod` x == 0]


isPrime :: Int -> Bool
isPrime n 
  | n > 1     = (length (divisor n)) == 2
  | otherwise = error "expected positive number of n > 1"


-- Exercise 5.11 on page 82-83

    
matches :: Int -> [Int] -> [Int]
matches n list = [x | x <- list, x == n]


elem' :: Int -> [Int] -> Bool
elem' n [] = False
elem' n (x:xs) = x == n || (elem' n xs)


-- Exercise 5.18 on page 90

shift ((x, y), z) = (x, (y, z))
-- :type shift is: `shift :: ((a1, a2), b) -> (a1, (a2, b))`


-- Exercise 5.22 on page 94
onSeparateLines :: [String] -> String
onSpearateLines [str] = str
onSeparateLines (str:list) = str ++ "\n" ++ (onSeparateLines list)
onSeparateLines [] = ""

-- Exercise 5.23 on page 94
duplicate :: String -> Int -> String
duplicate str 0 = ""
duplicate str n = str ++ (duplicate str (n - 1))

-- Exercise 5.24 on page 94
pushRight :: Int -> String -> String
pushRight linelength string = 
  let numSpaces = linelength - (length string) 
  in (replicate numSpaces ' ') ++ string

-- Chapter 6 --


-- Exercise 6.20-6.29 on page 112-113
-- !Do Exercise 7.2 on page 119

type Name = String
type Price = Int
type BarCode = Int
type Database = [(BarCode, Name, Price)]
type TillType = [BarCode]
type BillType = [(Name, Price)]

codeIndex :: Database
codeIndex = [ (4719, "Fish Fingers" , 121), 
              (5643, "Nappies" , 1010),
              (3814, "Orange Jelly", 56), 
              (1111, "Hula Hoops", 21),
              (1112, "Hula Hoops (Giant) " , 133), 
              (1234, "Dry Sherry, 1lt", 540)]

lineLength :: Int
lineLength = 30

look :: Database -> BarCode -> (Name, Price)
look database barCode = 
  case [(name, price) | (code, name, price) <- database, code == barCode] of
    []   -> ("Unkown Item", 0)
    list -> head list
  


lookup' :: BarCode -> (Name, Price)
lookup' barCode = look codeIndex barCode


formatPence :: Price -> String
formatPence price = 
  let poundsStr = (show (price `div` 100))
      pence = (price `mod` 100)
      penceStr = if pence < 10 
        then "0" ++ (show pence)
        else (show pence)
  in poundsStr ++ "." ++ penceStr


formatTotal :: Price -> String
formatTotal price = 
  let priceStr = (formatPence price)
  in "\nTotal" ++ (replicate (lineLength - 5 - (length priceStr)) '.') ++ priceStr ++ "\n"


formatLine :: (Name, Price) -> String
formatLine (name, price) = 
  let priceString = (formatPence price)
      numDots = lineLength - (length name) - (length priceString)
  in name ++ (replicate numDots '.') ++ priceString ++ "\n"


makeTotal :: BillType -> Price
makeTotal [] = 0
makeTotal (x:xs) = snd x + (makeTotal xs)


makeDiscount :: BillType -> Int
makeDiscount bill = ((length [x | (x, _) <- bill, x == "Dry Sherry, 1lt"]) - 1) * 100

formatDiscount :: Int -> String
formatDiscount discount = 
  let discountStr = (formatPence discount)
  in "\nDiscount" ++ (replicate (lineLength - 8 - (length discountStr)) '.') ++ discountStr ++ "\n"



formatBill :: BillType -> String
formatBill bill = 
  let discount = (makeDiscount bill)
      total = (makeTotal bill) - discount
  in (formatLines bill) ++ (formatDiscount discount) ++ (formatTotal total)
  where
    formatLines [] = ""
    formatLines (x:xs) = (formatLine x) ++ (formatLines xs)

makeBill :: TillType -> BillType
makeBill barCodes = [lookup' x | x <- barCodes]



produceBill :: TillType -> String
produceBill till = (formatBill [lookup' x | x <- till])



-- Chapter 7 --

-- Exercise 7.2 on page 119
addFirstTwoElem :: [Int] -> Int
addFirstTwoElem (x:y:xs) = x + y 
addFirstTwoElem [x] = x
addFirstTwoElem [] = 0

-- Exercise 7.3 on page 119
fstPlusOne' :: [Int] -> Int
fstPlusOne' list =
  if (length list) >= 1
  then (head list) + 1
  else 0

addFirstTwoElem' :: [Int] -> Int
addFirstTwoElem' list =
  if (length list) >= 2
  then (list !! 0) + (list !! 1)
  else if (length list == 1)
       then (list !! 0)
       else 0


-- Exercise 7.4 on page 120
product' :: [Int] -> Int
product' [] = 1 -- Must return 1 otherwise the entire product will be 0.
product' (x:xs) = x * (product xs)

-- ...extra using foldr
productFoldr' :: [Int] -> Int
productFoldr' list = foldr (*) 1 list

sumFoldr' :: [Int] -> Int
sumFoldr' list = foldr (+) 0 list


-- Exercise 7.5 on page 120
and', or' :: [Bool] -> Bool
and' list = foldr (&&) True list
or' list = foldr (||) False list


-- Exercise 7.7 on page 125
unique :: [Int] -> [Int]
unique list = [x | x <- list, isUnique x list]
  where
    isUnique a list = (length [x | x <- list, a == x]) == 1


-- Exercise 7.8 on page 125
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

unzip' :: [(a, b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((x, y):list) = 
  let unzipped = unzip' list
  in ([x] ++ (fst unzipped), [y] ++ (snd unzipped))

-- Exercise 7.9 on page 125
iSort :: [Int] -> [Int]
iSort []     = []
iSort (x:xs) = ins x (iSort xs)

ins :: Int -> [Int] -> [Int]
ins x [] = [x]
ins x (y:ys)
  | x <= y    = x:(y:ys)
  | otherwise = y : ins x ys

minMax :: [Int] -> (Int, Int)
minMax unsorted =
  let sorted = iSort unsorted
  in (head sorted, last sorted)

-- without iSort
minMax' :: [Int] -> (Int, Int)
minMax' list = (minList list, maxList list)
  where 
    minList [x] = x
    minList (x:xs) = min x (minList xs)
    maxList [x] = x
    maxList (x:xs) = max x (maxList xs)

-- Exercise 7.14 on page 128
drop' :: Int -> [a] -> [a]
drop' n xs
  | 0 < n = unsafeDrop' n xs
  | otherwise = []
  where
    unsafeDrop' :: Int -> [a] -> [a]
    unsafeDrop' _ [] = []
    unsafeDrop' 1 xs = [(last xs)]
    unsafeDrop' m xs = (unsafeDrop' (m - 1) (init xs)) ++ [(last xs)]

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs
  | 0 < n = unsafeSplitAt' n xs
  | otherwise = ([], [])
  where
    unsafeSplitAt' :: Int -> [a] -> ([a], [a])
    unsafeSplitAt' _ [] = ([], [])
    unsafeSplitAt' 1 (x:xs) = ([x], xs)
    unsafeSplitAt' m (x:xs) = 
      let (l, r) = unsafeSplitAt' (m - 1) (init xs)
      in (x : l, r ++ [last xs])

-- Exercise 7.18 on page 128


-- Exercise 7.19-7.23 on page 129-133 (harder)


-- Exercise 7.25 on page 133 


-- Exercise 7.26 on page 133 (harder..??)
