
--TURTLE EXAM EX

--PART 1--
data Turtle = Finished | Turn Double Turtle | Step Double Turtle
    deriving Show

data State = Current Point Double

done :: Turtle 
done = Finished

turn :: Double -> Turtle
turn alpha = Turn alpha Finished

step :: Double -> Turtle
step d = Step d Finished

(>>>) :: Turtle -> Turtle -> Turtle
Turn a t1 >>> t2 = Turn a (t1 >>> t2)
Step d t1 >>> t2 = Step d (t1 >>> t2)
(>>>) Finished t1 = t1
(>>>) t1 Finished = t1

square :: Turtle
square = foldl (>>>) Finished (replicate 4 (step 50 >>> turn 90)) 

--PART 2--
type Point = (Double,Double)
type Line = (Point,Point)

turtleToLine :: Turtle -> [Line]
turtleToLine = go (Current (500.0,500.0) 0)

go :: State -> Turtle -> [Line]
go (Current (x,y) a) Finished = []
go (Current (x,y) alpha) (Turn a t) = go (Current (x,y) (alpha+a)) t
go (Current (x,y) a) (Step d t) = ((x,y), newPoint) : go (Current newPoint a) t
    where newPoint = newPos (x,y) a d


newPos :: (Double,Double) -> Double -> Double -> (Double,Double)
newPos (x,y) alpha d = (newX,newY) 
    where newX = x + d * sin (alpha * 2 * pi / 360.0)
          newY = y + d * cos (alpha * 2 * pi / 360.0)

linesToSVG :: [Line] -> String 
linesToSVG lines = header ++ body ++ end
    where header = "<svg xmlns=\"https://www.w3.org/2000/svg\" version=\"1.1\">"
          body = unlines (map lineToSVG lines)
          end = "</svg>"

lineToSVG :: Line -> String 
lineToSVG ((x1,y1),(x2,y2)) = "<line x1=\"" ++ show x1 ++ "\" y1=\"" 
        ++ show y1 ++ "\" x2=\"" ++ show x2 ++  "\" y2=\"" ++ show y2 ++
        "\" stroke=\"blue\" stroke-width=\"4\" />"

squareSVG :: String 
squareSVG = linesToSVG (turtleToLine square)

writeSVG :: FilePath -> Turtle -> IO()
writeSVG path t = writeFile path (linesToSVG (turtleToLine t))

--PART 3--
data Fractal = Fdone | Fturn Double Fractal | Fstep Fractal
    deriving Show

--data Fstate = Current Point Double

fdone :: Fractal 
fdone = Fdone

fturn :: Double -> Fractal
fturn alpha = Fturn alpha Fdone

fstep :: Fractal
fstep = Fstep Fdone

(>->) :: Fractal -> Fractal -> Fractal
Fturn a t1 >-> t2 = Fturn a (t1 >-> t2)
Fstep t1 >-> t2 = Fstep(t1 >-> t2)
Fdone >-> t1 = t1

concretize :: Double -> Fractal -> Turtle
concretize d (Fturn a t1) = Turn a (concretize d t1)
concretize d (Fstep t1) = Step d (concretize d t1)
concretize d Fdone = Finished

--refine :: Fractal -> Fractal -> Fractal
