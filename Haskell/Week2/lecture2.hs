module Template where

idListInt :: [Int] -> [Int]
idListInt = foldr (:) []

--idListInt :: [Int] -> [Int]
--idListInt (x:xs) = c x (idListInt xs) = (:) x (idListInt xs)

addAndCount :: [Int] -> (Int,Int)
addAndCount = foldr c n
    where
        n = (0,0)
        c x (c,s) = (1+c,x+s)

split' [] = ([],[])
split' (x:xs) = (zs,x:ys)
    where
        (ys,zs) = split' xs

split :: [Int] -> ([Int],[Int])
split = foldr c n
    where
        n = ([],[])
        c x (l1,l2) = (l2, x:l1)

-- > (tuple [1,2,3]) 0
--          [(1,0),(2,0),(3,0)]
tuple :: [a] -> (b -> [(a,b)])
tuple l = foldr c n l
    where
        n = \ y -> []
        c x r = \ y -> (x,y) : r y

tails :: [a] -> [[a]]
tails = foldr c n
    where
        n = [[]]
        c x (l1:ls) = (x:l1) : (l1:ls)
                    -- list : list(list)
    --  c x r = (x : head r) : r
--c 1 [[2,3,4,5],[3,4,5],[4,5],[5],[]] = [[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5],[]]

--Maximal tail sum 
mts :: [Int] -> Int 
--mts l = maximum (map sum (tails l))
--mts l = (maximum . map sum . tails) l
mts = maximum . map sum . tails 
--mts l = maximum . map sum $ tails l

--mtsOneLoop :: [Int] -> Int -> Int 


data Tree a = MkLeaf a | MkTree (Tree a) (Tree a)

foldTree :: (r -> r -> r) -> (a -> r) -> Tree a -> r
foldTree f n (MkLeaf x) = n x
foldTree f n (MkTree right left) = f (foldTree f n right) (foldTree f n left)