
import Data.Char (chr)
import Data.Char (ord)
import Distribution.Simple.Utils (xargs)
-------TREE FOLD


data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree fLeaf fTree (Leaf x) = fLeaf x
foldTree fLeaf fTree (Fork left rigth) = fTree (foldTree fLeaf fTree left) (foldTree fLeaf fTree rigth)

--sumTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) = 6
sumTree :: Tree Int -> Int
sumTree = foldTree (id) (+)

--treeToList (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) = [1,2,3]
treeToList :: Tree a -> [a]
treeToList tree = foldTree (\x-> [x]) (++) tree

--nrOfLeaves (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3))) = 3
nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (\x-> 1) (+)

--depthOfTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3)))
depthOfTree :: Tree a -> Int
depthOfTree = foldTree (\x-> 1) (\x y-> (max x y)+1)

--mirrorTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3)))
mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree (\x-> Leaf x) (\x y-> Fork y x)

--minTree (Fork (Fork (Leaf 20) (Leaf 30)) (Leaf 10) )
minTree :: Tree Int -> Int
minTree = foldTree id (\x y-> min x y)

--addOne (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3)))
-- =  Fork (Leaf 2) (Fork (Leaf 3) (Leaf 4
addOne :: Tree Int -> Tree Int
addOne = foldTree (\x-> Leaf (x+1)) (\x y-> Fork x y)

--idTree (Fork (Leaf 1) (Fork (Leaf 2) (Leaf 3)))
idTree :: Tree a -> Tree a
idTree = foldTree (\x-> Leaf x) (\x y-> Fork x y)



----EX2----------------------------------------------------

-- * Rectangles and Squares
-- ----------------------------------------------------------------------------

rectangle :: Int -> Int -> IO ()
rectangle x 0 = return ()
rectangle x y =
    do
        putStrLn (replicate x '*')
        rectangle x (y-1)


square :: Int -> IO ()
square dim = rectangle dim dim

-- * Trapezoids and Triangles
-- ----------------------------------------------------------------------------


trapezoid :: Int -> Int -> IO ()
trapezoid w h = trapezoidLevel w h 1

trapezoidLevel :: Int -> Int -> Int  -> IO ()
trapezoidLevel w h level =
    if h == level then
        do
            putStrLn (replicate (w + 2*(h-1)) '*')
    else
        do
            putStr (replicate (h-level) ' ')
            putStrLn (replicate (w + 2*(level-1)) '*')
            trapezoidLevel w h (level+1)


triangle :: Int -> IO ()
triangle base = trapezoid 1 base


---EX3------------------------------------------

data State =
      Init
    | Num Int
    | Add Int
    | Sub Int
    deriving (Eq, Show)

repr :: State -> String
repr Init = ""
repr (Num x) = show x
repr (Add x) = show x ++ " +"
repr (Sub x) = show x ++ " -"

isInt :: String -> Bool
isInt [] = False
isInt (c:cs) =
    if c == '-' then
        not (length cs == 0) &&
        foldr (\char bool -> (isDigit char) && bool) True cs
    else
        foldr (\char bool -> (isDigit char) && bool) True (c:cs)


--already defined in library
isDigit :: Char -> Bool
isDigit c = (asciiVal > 47) && (asciiVal < 58)
    where asciiVal = ord c


transition :: State -> String -> State
transition Init str =
    if isInt str then Num (read str)
    else Init
transition _ "reset" = Init
transition (Num x) str =
    case str of
        "+" -> Add x
        "-" -> Sub x
        _ -> if isInt str then Num (read str)
            else Num x
transition (Add x) str =
    if isInt str then
        Num (x + read str)
    else
        case str of
            "-" -> Sub x
            _ -> Add x
transition (Sub x) str =
    if isInt str then
        Num (x - read str)
    else
        case str of
            "+" -> Add x
            _ -> Sub x




calculator :: IO ()
calculator = 
    do 
        putStrLn ""
        calculatorLoop Init
        putStrLn "Goodbye :)"

calculatorLoop :: State -> IO ()
calculatorLoop oldState =
    do 
        line <- getLine 
        if line == "exit" then 
            return ()
        else
            do
                let newState = transition oldState line :: State
                putStrLn (repr newState)
                calculatorLoop newState