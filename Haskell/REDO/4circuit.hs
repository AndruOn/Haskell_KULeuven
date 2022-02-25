module MyHaskell where
import Data.List

-- Task 1a

data Circuit
  = Input String
  | NOT Circuit
  | AND  Circuit Circuit
  | OR Circuit Circuit
  | XOR Circuit Circuit

-- Task 1b

cinput :: String -> Circuit 
cinput = Input

cnot   :: Circuit -> Circuit
cnot   = NOT

cand   :: Circuit -> Circuit -> Circuit
cand   = AND

cor    :: Circuit -> Circuit -> Circuit
cor    = OR

cxor   :: Circuit -> Circuit -> Circuit
cxor   = XOR

-- Task 1c

example :: Circuit
example = OR (AND (Input "x") (Input "y")) (XOR (NOT (Input "z")) (Input "x"))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany [c] = c
candMany (c:cs) = AND c (candMany cs)

-- Task 2a

instance Show Circuit where
  show (Input s) = s 
  show (NOT x) = "NOT(" ++ show x ++ ")"
  show (AND x y) = "AND(" ++ show x ++ "," ++ show y ++ ")"
  show (OR x y) = "OR(" ++ show x ++ "," ++ show y ++ ")"
  show (XOR x y) = "XOR(" ++ show x ++ "," ++ show y ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (OR x y) = NOT (AND (NOT (simplify x)) (NOT (simplify y)))
simplify (XOR x y) = simplify (OR (AND x (NOT y)) (AND (NOT x) y))
simplify x = x

-- Task 2c

size :: Circuit -> Int
size (Input x) = 0
size (NOT x) = 1 + size x
size (AND x y) = 1 + size x + size y
size (OR x y) = 1 + size x + size y
size (XOR x y) = 1 + size x + size y

-- Task 2d

gateDelay :: Circuit -> Int
gateDelay (Input x) = 0
gateDelay (NOT x) = 1 + gateDelay x
gateDelay (AND x y) = 1 + max (gateDelay x) (gateDelay y)
gateDelay (OR x y) = 1 + max (gateDelay x) (gateDelay y)
gateDelay (XOR x y) = 1 + max (gateDelay x) (gateDelay y)

-- Task 2e

inputs :: Circuit -> [String]
inputs (Input s) = [s]
inputs (NOT x) = inputs x
inputs (AND x y) = nub (inputs x ++ inputs y)
inputs (OR x y) = nub (inputs x ++ inputs y)
inputs (XOR x y) = nub (inputs x ++ inputs y)

-- Task 3a

simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input s) var = getVal s var
simulate (NOT x) var   = not $ simulate x var
simulate (AND x y) var = simulate x var && simulate y var
simulate (OR x y) var  = simulate x var || simulate y var
simulate (XOR x y) var = (bx && not by) || (not bx && by)
    where
        bx = simulate x var
        by = simulate y var

getVal :: String -> [(String,Bool)] -> Bool
getVal var ((s,b):ps) = 
    if var == s then 
        b 
    else 
        getVal var ps

-- Task 3b

combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = [b:bs | b <- [False,True], bs <- combinations (n-1)]

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate c = 
    do 
        let ins = inputs c :: [String]
        mapM_ (putStr . (\x -> x ++ " ")) ins
        putStrLn "| output"
        let assignations = format (combinations (length $ ins)) ins :: [[(String,Bool)]]
        loop c assignations

format :: [[Bool]] -> [String] -> [[(String,Bool)]]
format [] _ = []
format (bs:bss) vars = zip vars bs : format bss vars

loop :: Circuit -> [[(String,Bool)]] -> IO ()
loop _ [] = return ()
loop c (ps:pss)= 
    do
        mapM_ (putStr . (\p -> show (boolToInt (snd p)) ++ " ")) ps
        putStrLn ("| " ++ show(boolToInt (simulate c ps)))
        loop c pss

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

-- -- Task 4a
-- 
-- check :: Circuit -> [(String,Bool)] -> Bool -> Bool
-- check c env r = undefined
-- 
-- checkAll :: [([(String,Bool)],Bool)] -> Circuit -> Bool
-- checkAll = undefined
-- 
-- -- Task 4b
-- 
-- splits :: Int -> [(Int,Int)]
-- splits = undefined
-- 
-- -- Task 4c
-- 
-- generate :: [String] -> Int -> [Circuit]
-- generate = undefined
-- 
-- -- Task 4d
-- 
-- smallest :: [String] -> [([(String,Bool)],Bool)] -> Circuit
-- smallest = undefined
  
