module Template where
    
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert y (x:xs) =
    if y <= x then
        (y:x:xs)
    else
        (x:insert y xs)

isort :: [Int] -> [Int]
isort x = []

-- * Function Chaining
-- ----------------------------------------------------------------------------

applyAll :: [a -> a] -> a -> a
applyAll fns x = (foldr (.) id fns) x

applyTimes :: Int -> (a -> a) -> a -> a
applyTimes n fn x = applyAll (replicate n (repeat fn)) x

applyMultipleFuncs :: a -> [a -> b] -> [b]
applyMultipleFuncs x fns = [fn x | fn <- fns]



{-
-- mergeSort
split :: [Int] -> ([Int],[Int])
merge :: [Int] -> [Int] -> [Int]
mergesort :: [Int] -> [Int]
-}