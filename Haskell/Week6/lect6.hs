import Data.List
import Data.Set (elemAt)

--(!!) and cycle

testData :: [Int]
testData = [1721, 979, 366, 299, 675 ,1456]

solve :: [Int] -> Int
solve list = head [ x*y | (x:list') <- tails list , y <- list', x+y == 2020]

expensesSum2 :: FilePath -> IO()
expensesSum2 path =
    do
        file <- readFile path
        let numbers = map read (lines file) :: [Int]
        print (solve numbers)

expensesSum3 :: FilePath -> IO()
expensesSum3 path =
    do
        file <- readFile path
        let list = map read (lines file) :: [Int]
        print $ head [ x*y | (x:list') <- tails list , (y:list'') <- tails list', z <- list'', x+y+z == 2020]

testdata2 :: [String]
testdata2 =
  ["..##.......",
   "#...#...#..",
   ".#....#..#.",
   "..#.#...#.#",
   ".#...##..#.",
   "..#.##.....",
   ".#.#.#....#",
   ".#........#",
   "#.##...#...",
   "#...##....#",
   ".#..#...#.#"]

--doent workk :'(
horseCount :: [String] -> Int -> Int
horseCount [] _ = 0
horseCount (s:ss) pos =  if (!!) s newPos == '#' then 1 + recall else recall
    where newPos = mod (pos+3) (length s) :: Int
          recall = horseCount ss newPos :: Int

--cycle (laziness of haskell only ompute when its needed)
shorterCount :: [String] -> Int 
shorterCount ls = 
    length $ filter (== '#') $ go $ map cycle ls
    where
        go [] = []
        go ((x:_):xxs) = x : go (map (drop 3) xxs)
