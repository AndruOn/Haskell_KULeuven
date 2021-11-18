module Template where

-- * Haskell 101
-- ----------------------------------------------------------------------------

double :: Int -> Int
double x = 2 * x

myAbs :: Int -> Int
myAbs x =
    if x>=0
        then x
        else -x

toFahrenheit :: Float -> Float
toFahrenheit f = 1.8*f + 32.0

fizzbuzz :: Int -> String
fizzbuzz x
    | (mod x 3 == 0) && (mod x 5 == 0)  = "fizzbuzz"
    | mod x 3 == 0 = "fizz"
    | mod x 5 == 0 = "buzz"
    | otherwise = show x



