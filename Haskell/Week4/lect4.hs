import Control.Monad 

--import System.Random 

-- program that prints number 5
print5 :: IO ()
print5 = print 5 

--program that prints the name
printName :: IO ()
printName = putStrLn "Andru"

-- 1.a  Write an IO program which will first read a positive
--      integer, say n, and then reads n integers and writes
--      their sum.

copyNInt :: IO ()
copyNInt = do
                n <- readLn 
                listN <- readNInt n
                print (sum listN)

readNInt :: Int -> IO [Int]
readNInt 0 = return []
readNInt n = do
                v <- readLn 
                vs <- readNInt (n-1)
                return (v:vs)

--ATTENTION replicateM SUPER IMPORTANT
copyNIntShort :: IO ()
copyNIntShort = 
    do 
        n <- readLn 
        xs <- replicateM n readLn 
        print (sum xs)
                    

-- 1.b  Modify the program to read the numbers from a file.
--      Use the predefined functions:
--        readFile :: FilePath -> IO String
--           (where type FilePath = String)
--        unlines  :: String -> [String]

prog1b = 
    do
        ls <- readFile  "data.txt"
        print(lines ls)

sumNIntFromFile :: FilePath -> IO ()
sumNIntFromFile filePath = 
    do 
        file <- readFile filePath
        let numbersStr = lines file :: [String]
        let (n:numbers) = map read numbersStr :: [Int]
        print (sum (take n numbers))

--example of ambigous program: (show . read) "1"
--     if youspecify you will get diff results   show (read "1" :: Int)  gives "1"
--                                               show (read "1" :: Float) gives "1.0"


-- 1.c  Modify the  program to write the sum to a file.
--      Use the predefined function:
--        writeFile :: FilePath -> String -> IO ()

--prog1c "data.txt" "output"
prog1c :: FilePath -> FilePath -> IO ()
prog1c readPath writePath= 
    do 
        file <- readFile readPath
        let numbersStr = lines file :: [String]
        let (n:numbers) = map read numbersStr :: [Int]
        writeFile writePath (show (sum (take n numbers)))

-- 2.a Write a number guessing game.
--     The user thinks of a number and the game guesses it
--     in a number of attempts.
--
--      Main> game
--      Think of a number between 1 and 100!
--      Is it 50? higher
--      Is it 75? lower
--      Is it 62? lower
--      Is it 56? yes
--      Great, I won!


game :: IO ()
game = 
    do
        putStr "Think of a number between 1 and 100!\n"
        recursiveLoop 1 100
        putStr "Great I won\n"
        
--ATTENTION do nothing is "return ()"
recursiveLoop :: Int -> Int -> IO ()
recursiveLoop low high =
    do
        let middle = (low + high) `div` 2
        putStr ("Is it " ++ (show middle) ++ "? ") 
        answer <- getLine 
        case answer of
            "higher" -> recursiveLoop (middle+1) high
            "lower" -> recursiveLoop low (middle-1)
            "yes" -> return ()
            _ -> do 
                putStrLn "Please respond with higher, lower or yes" 
                recursiveLoop low high 

-- 2.b Invert the game: the program thinks of a number
--     between 1 and 100, and the user guesses it.
--
--     Import System.Random and use
--       randomRIO :: (Int,Int) -> IO Int
--     to get a random integer in the given range.
gameGuess :: IO ()
gameGuess = 
    do
        putStr "Guess the number I am thinking of between 1 and 100!\n"
        let n = 69 :: Int --n <- randomRIO (1,100)
        loopGuess n
        putStr "Great you won\n"   

loopGuess :: Int -> IO ()
loopGuess n =
    do 
        putStrLn "What is your guess?"
        guess <- readLn 
        case compare n guess of
            LT -> do
                    putStr "lower"
                    loopGuess n
            EQ -> return ()
            GT -> do
                    putStr "higher"
                    loopGuess n





-------REDO------------------------------------------------------------------
-- 2.a Write a number guessing game.
--     The user thinks of a number and the game guesses it
--     in a number of attempts.
--
--      Main> game
--      Think of a number between 1 and 100!
--      Is it 50? higher
--      Is it 75? lower
--      Is it 62? lower
--      Is it 56? yes
--      Great, I won!
game' :: IO ()
game' = 
    do
        putStrLn "Think of a number between 1 and 100!"
        loop 1 100
        putStrLn "Great I won !"

loop :: Int -> Int -> IO ()
loop start end = 
    do
        let guess = div (start + end) 2 :: Int
        putStr ("Is it " ++ show guess ++ "? ")
        answer <- getLine
        case answer of 
            "higher" -> loop (guess + 1) end
            "lower" -> loop start (guess - 1)
            "yes" -> return ()
            _ -> do
                    putStrLn "ERROR input: type higher lower or yes"
                    loop start end