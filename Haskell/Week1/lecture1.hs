
module Template where
import Distribution.SPDX (LicenseId(XSkat))

add :: [Int] -> Int
add xs = foldr (+) 0 xs

count :: [Int] -> Int
count xs = foldr (\ x -> (+) 1) 0 xs

averageRunTImeException :: [Int] -> Int
averageRunTImeException list =  add list `div` count list

averageInt :: [Int] -> Int
averageInt [] = 0
averageInt list = add list `div` count list

-- data Maybe a = Nothing | Just a

average2 :: [Int] -> Maybe Int
average2 [] = Nothing
average2 list = Just (add list `div` count list)

average :: [Int] -> Maybe Double
average [] = Nothing
average list = Just (fromIntegral(add list) / fromIntegral(count list))

averageOneLoopAcc :: [Int] -> Double
averageOneLoopAcc list = averageHelper list 0.0 0.0
    where
        averageHelper :: [Int] -> Double -> Double -> Double
        averageHelper [] nb sum = sum / nb
        averageHelper (x:xs) nb sum = averageHelper xs (nb+1) (sum+fromIntegral x)

averageOneLoop :: [Int] -> Double
averageOneLoop list = let (c,s) = addAndCount list
                      in fromIntegral s / fromIntegral c
    where
        addAndCount :: [Int] -> (Int,Int)
        addAndCount [] = (0,0)
        addAndCount (x:xs) = addTuple (1,x) (addAndCount xs)

        addTuple :: (Int,Int) -> (Int,Int) -> (Int,Int)
        addTuple (x,y) (a,b) = (x+a,y+b)

insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (x:xs) a =
    if a>x
        then x:(insert xs a)
        else a:x:xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

-- data Either a b = Left a | Right b
-- data Either String a = Nobug String | ErrorWrongType a

--LET IN and WHERE
f x = let y = x + 1
          z = x + 2
    in x * y * z

f' x =  x*y*z
    where
        y =  x+1
        z = x + 2

-- mergeSort
split :: [Int] -> ([Int],[Int])
split [] = ([],[])
split [x] = ([x],[])
split (x1:x2:xs) = (x1:ys,x2:zs)
    where
        (ys,zs) = split xs

split' [] = ([],[])
split' (x:xs) = (zs,x:ys)
    where
        (ys,zs) = split xs

merge :: [Int] -> [Int] -> [Int]
merge l [] = l
merge [] l = l
merge (x:xs) (y:ys) =
    if x < y
        then x : merge xs (y:ys)
        else y : merge (x:xs) ys

mergesort :: [Int] -> [Int]
mergesort list = merge small big
    where (xs,ys) = split' list
          small = mergesort xs 
          big = mergesort ys
          