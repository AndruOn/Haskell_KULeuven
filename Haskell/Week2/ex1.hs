
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

myproduct :: [Integer] -> Integer
myproduct = foldr (*) 1

insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert y (x:xs) = 
    if y <= x then
        (y:x:xs)
    else
        (x:insert y xs)

myLast :: [Int] -> Int
myLast [x] = x
myLast (x:xs) = myLast xs

-- * Rock - Paper - Scissors
-- ----------------------------------------------------------------------------

data Move = Rock | Paper | Scissors
  deriving (Eq, Show)

beat :: Move -> Move
beat Scissors = Rock 
beat Rock = Paper 
beat Paper = Scissors

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose Scissors = Paper

data Result = Win | Lose | Draw
  deriving (Eq, Show)

outcome :: Move -> Move -> Result
outcome m1 m2 
    | beat m1 == m2 = Lose
    | lose m1 == m2 = Win
    | otherwise = Draw

-- * List Operations
-- ----------------------------------------------------------------------------

factorial :: Integer -> Integer
factorial n = 
  if n < 0 then 1
  else product [1..n]

myRepeat :: Int -> Int -> [Int]
myRepeat n x = 
  if n < 0 then []
  else [x | _ <- [1..n]]

flatten :: [[Int]] -> [Int]
flatten superList = [a | list <- superList , a <- list]

range :: Int -> Int -> [Int]
range low high = [low..high]

sumInts :: Int -> Int -> Int
sumInts low high = sum (range low high) 

removeMultiples :: Int -> [Int] -> [Int]
removeMultiples n list = [x | x <- list , (x `mod` n) /= 0]

-- * List Comprehensions
-- ----------------------------------------------------------------------------

mapLC :: (a -> b) -> [a] -> [b]
mapLC f list = [ f a | a <- list]

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC f list = [ a | a <- list , f a]

-- * Folds
-- ----------------------------------------------------------------------------

mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Integer] -> Integer
myProduct = foldInts (*) 1 

foldInts :: (Integer -> Integer -> Integer) -> Integer -> [Integer] -> Integer
foldInts _ base [] = base
foldInts fn base (i:ints) = fn i (foldInts fn base ints)

-- myFoldl (++) "" ["Hello", " ", "World"] = "Hello World"
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ base [] = base
myFoldl fn base (x:xs) = myFoldl fn (fn base x) xs

-- myFoldr (++) "" ["Hello", " ", "World"] = "Hello World"
-- myFoldr (-) [1..4] = -2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ base [] = base
myFoldr fn base (x:xs) = fn x (myFoldr fn base xs) 

--doesnt work yet
myDifl :: [Integer] -> Integer -> Integer
myDifl [] base = base
myDifl (x:xs) base = newbase - (myDifl xs newbase)
  where newbase = base - x

myDifr :: [Integer] -> Integer
myDifr [] = 0
myDifr (x:xs) = x - mySum xs



-- readInBase 6 [1,3,1] = 6 * 6 * 1 + 6 * 3 + 1 = [ 6 * ( 6* ( 6 * 1 + 3) + 1) ] / 6
-- readInBase 6 [1,2] = 6 * 1 + 1 = 6 * (6 * 1 + 2)  / 6 = 8
readInBase :: Int -> [Int] -> Int
readInBase base digits = foldl fn 0 digits
  where fn x b = base * x + b

myMap :: (a -> b) -> [a] -> [b]
myMap fn list = [fn x | x <- list]

myMapF :: (a -> b) -> [a] -> [b]
myMapF fn = foldr (\x y-> fn x : y) [] 