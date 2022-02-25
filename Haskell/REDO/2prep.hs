
module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)

length' :: [Int] -> Int
length' = foldr (\x y -> y + 1) 0 

any' :: (Int -> Bool) -> [Int] -> Bool
any' fn = foldr ((||).fn) False 

all' :: (Int -> Bool) -> [Int] -> Bool
all' fn = foldr ((&&).fn) True 

map' :: (Int -> Int) -> [Int] -> [Int]
map' fn = foldr (\x y -> fn x : y) [] 

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' fn = foldr (\x y -> if fn x then x : y else y) [] 

-- * Given helpers

even' :: Int -> Bool
even' = even

not' :: Bool -> Bool
not' = not

absolute' :: Int -> Int
absolute' = abs

greaterThanFive :: Int -> Bool
greaterThanFive x = x > 5

-- * Beginning Composer

amountEven :: [Int] -> Int
amountEven = length.(filter' even)

onlyOdd :: [Int] -> [Int]
onlyOdd = filter' (not.even)

absGtFive :: [Int] -> Int
absGtFive =  length.(filter' ((>5).abs))

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive xs = any (>5) (filter' even xs)

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = foldr' ((||). greaterThanFive) False . filter even'
