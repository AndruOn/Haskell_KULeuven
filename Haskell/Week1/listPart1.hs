
module Template where

-- * List Operations
-- ----------------------------------------------------------------------------

count :: [Int] -> Int
count [] = 0
count (x:xs) = 1 + count xs

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
    if x == True
        then myAnd xs
        else False

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x == False
        then myOr xs
        else True

append :: [Int] -> [Int] -> [Int]
append [] list = list
append list [] = list
append (x:xs) (y:ys) = append (x:addlast xs y) ys

addlast :: [Int] -> Int -> [Int]
addlast [] x = [x]
addlast (x:xs) y = x:addlast xs y
