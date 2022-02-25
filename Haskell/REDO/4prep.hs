module Template where
import Control.Monad

-- * Drilling on IO
-- ----------------------------------------------------------------------------
printEachline :: Show a => [a] -> IO ()
printEachline [] = return ()
printEachline (x:xs) = 
    do
        putStrLn $ show x --or: print x
        printEachline xs


prog1 :: IO ()
prog1 = 
    do 
        m <- readLn :: IO Int
        n <- readLn :: IO Int
        let xs = replicate m n
        printEachline xs

prog1' :: IO ()
prog1' = 
    do
        m <- readLn :: IO Int
        n <- readLn :: IO Int
        replicateM_ m (print n)

prog1b :: IO ()
prog1b =  
    (readLn :: IO Int) >>= \m ->
    (readLn :: IO Int) >>= \n ->
    replicateM_ m (print  n)

prog2 :: IO ()
prog2 = 
    do 
        s <- getLine
        if s=="" then 
            return () 
        else
            do
                print (reverse s)
                prog2

index :: [IO a] -> IO Int -> IO a
index list ioInt = 
    do
        i <- ioInt :: IO Int
        list!!i

