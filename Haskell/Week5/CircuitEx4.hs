
import Data.List
import Foreign
-- Task 1a

data Circuit
  = Input String |
  NOT Circuit |
  AND Circuit Circuit |
  OR Circuit Circuit |
  XOR Circuit Circuit


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
example = cor 
    (cand (cinput "x") (cinput "y")) 
    (cxor (cinput "x") (cnot (cinput "z")))

-- Task 1d

candMany :: [Circuit] -> Circuit
candMany [c] = c
candMany (c:cs) = AND c (candMany cs)

-- Task 2a

instance Show Circuit where
  show (Input x) = x
  show (NOT x) = "NOT(" ++ show x ++ ")"
  show (OR x y) = "OR(" ++ show x ++ "," ++ show y ++ ")"
  show (XOR x y) = "XOR(" ++ show x ++ "," ++ show y ++ ")"
  show (AND x y) = "AND(" ++ show x ++ "," ++ show y ++ ")"

-- Task 2b

simplify :: Circuit -> Circuit
simplify (Input x) = Input x
simplify (NOT x) = NOT (simplify x)
simplify (AND x y) = AND (simplify x) (simplify y)
simplify (OR x y) = NOT (AND 
                            (NOT (simplify x)) 
                            (NOT (simplify y))
                        )
-- dont need the simplfy for each x and y bc there is a simplify at the beginning
simplify (XOR x y) = simplify (OR 
                        (AND 
                            x
                            (NOT y)
                        )
                        (AND 
                            (NOT x)
                            y
                        )
                    )
                        

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
inputs c = nub (allInputs c)

allInputs :: Circuit -> [String]
allInputs (Input x) = [x]
allInputs (NOT x) = inputs x
allInputs (AND x y) = inputs x ++ inputs y
allInputs (OR x y) = inputs x ++ inputs y
allInputs (XOR x y) = inputs x ++ inputs y

-- Task 3a
-- simulate example [("x",True),("y",False),("z",True)] -> True
simulate :: Circuit -> [(String,Bool)] -> Bool
simulate (Input x) dic = b
    where Just b = lookup x dic
simulate (NOT x) dic =  not (simulate x dic)
simulate (AND x y) dic = simulate x dic && simulate y dic
simulate (OR x y) dic = simulate x dic || simulate y dic
simulate (XOR x y) dic = simulate x dic /= simulate y dic

-- Task 3b
combinations :: Int -> [[Bool]]
combinations 0 = [[]]
combinations n = [ b : bs | b <- [False,True] , bs <- combinations (n-1) ] 

-- Task 3c
tabulate :: Circuit -> IO ()
tabulate c = 
    do
        printVar c
        putStrLn "| output"
        let ins = inputs c :: [String]
        printBoolChoice c (assignements (combinations (length ins)) ins)

assignements :: [[Bool]] -> [String] -> [[(String,Bool)]]
assignements [] _ = []
assignements (bools:rest) inputs = zip inputs bools : assignements rest inputs


printVar :: Circuit -> IO()
printVar c = 
    do 
        putStr (foldr (\x y-> x++" "++y) "" (inputs c))

printBools :: [(String,Bool)] -> IO()
printBools [] = return ()
printBools ((var,bool):rest) = 
    do 
        putStr (show (fromBool bool) ++ " ")
        printBools rest


printBoolChoice :: Circuit -> [[(String,Bool)]] -> IO()
printBoolChoice c [] = return ()
printBoolChoice c (inBool : rest) = 
    do 
        printBools inBool
        putStrLn ("| " ++ show (fromBool (simulate c inBool)))
        printBoolChoice c rest

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
