module Template where

import Data.Char


-- * Tree folds
-- ----------------------------------------------------------------

data Tree a = Leaf a | Fork (Tree a) (Tree a)
  deriving (Show, Eq)

foldTree :: (a -> b) -> (b -> b -> b) -> (Tree a -> b)
foldTree fnLeaf fnFork (Fork t1 t2) = fnFork (foldTree fnLeaf fnFork t1) (foldTree fnLeaf fnFork t2)
foldTree fnLeaf fnFork (Leaf a) = fnLeaf a

sumTree :: Tree Int -> Int
sumTree = foldTree id (+) 

treeToList :: Tree a -> [a]
treeToList = foldTree (: []) (++) 

nrOfLeaves :: Tree a -> Int
nrOfLeaves = foldTree (const 1) (+) 

depthOfTree :: Tree a -> Int
depthOfTree = foldTree (const 1) (\x y -> 1  + max x y) 

mirrorTree :: Tree a -> Tree a
mirrorTree = foldTree Leaf (\t1 t2 -> Fork t2 t1)

minTree :: Tree Int -> Int
minTree = foldTree id min

addOne :: Tree Int -> Tree Int
addOne = foldTree (\x-> Leaf (x+1)) Fork  

idTree :: Tree a -> Tree a
idTree = foldTree Leaf Fork


-- * Rectangles and Squares
-- ----------------------------------------------------------------------------

rectangle :: Int -> Int -> IO ()
rectangle c 0 = return ()
rectangle c l =
    do
        putStrLn (concat (replicate c "*"))
        rectangle c (l-1)

{-
rectangle :: Int -> Int -> IO ()
rectangle c l =
    do
        let io = putStrLn (foldr (++) [] (replicate c "*"))
        replicateM_ l io
        
-}

square :: Int -> IO ()
square dim = rectangle dim dim

-- * Trapezoids and Triangles
-- ----------------------------------------------------------------------------

printLine :: Int -> Int -> IO ()
printLine n nSpaces = putStrLn (spaces ++ stars)
    where
        spaces = replicate nSpaces ' '
        stars = replicate n '*'

trapezoid :: Int -> Int -> IO ()
trapezoid _ 0 = return ()
trapezoid w h = 
    do 
        printLine w (h-1)
        trapezoid (w+2) (h-1)

triangle :: Int -> IO ()
triangle = trapezoid 1


-- * Calculator
-- ----------------------------------------------------------------------------

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
    if c == '-' && length cs > 0 then
        all isDigit cs
    else 
        all isDigit (c:cs)


transition :: State -> String -> State
transition _ "reset" = Init
transition Init s
    | isInt s = Num (read s) -- Init
    | otherwise = Init
transition (Num x) s
    | isInt s = Num (read s)
    | s == "+" = Add x
    | s == "-" = Sub x
    | otherwise = Num x
transition (Add x) s
    | isInt s = Num (x + read s) --Add
    | s == "-" = Sub x
    | otherwise = Add x
transition (Sub x) s
    | isInt s = Num (x - read s) -- Sub
    | s == "+" = Add x
    | otherwise = Sub x

calculator :: IO ()
calculator = 
    do 
        loop Init 
        putStrLn "Goodbye :)"

loop :: State -> IO ()
loop oldS = 
    do
        putStrLn (repr oldS)
        s <- getLine
        if s == "exit" then
            return ()
        else
            do
                let newS = transition oldS s :: State
                loop newS

