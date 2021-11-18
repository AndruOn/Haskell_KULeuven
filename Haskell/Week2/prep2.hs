
module Template where

foldr' :: (Int -> a -> a) -> a -> [Int] -> a
foldr' f z []       = z
foldr' f z (x : xs) = f x (foldr f z xs)


length' :: [Int] -> Int
length' = foldr (\ _ len_acc -> len_acc + 1) 0

any' :: (Int -> Bool) -> [Int] -> Bool
any' func = foldr' ((||).func) False

all' :: (Int -> Bool) -> [Int] -> Bool
all' func = foldr' ((&&).func) True

map' :: (Int -> Int) -> [Int] -> [Int]
map' = map

filter' :: (Int -> Bool) -> [Int] -> [Int]
filter' = filter

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
amountEven = length' . filter even'

onlyOdd :: [Int] -> [Int]
onlyOdd = filter (not . even')

absGtFive :: [Int] -> Int
absGtFive = length' . filter' (greaterThanFive . absolute')

anyEvenGtFive :: [Int] -> Bool
anyEvenGtFive = any' greaterThanFive . filter even'

anyEvenGtFive' :: [Int] -> Bool
anyEvenGtFive' = foldr' ((||). greaterThanFive) False . filter even'
