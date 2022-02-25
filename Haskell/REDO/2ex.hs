
module Template where
import Data.List 
import Data.Char

-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Integer where
  prev x = x-1
  next x = x+1

instance Sequence Char where
  prev c = chr ((ord c)+1)
  next c = chr ((ord c)-1)
  --next 'z' = throw "no value after 'z' "

instance Sequence Bool where
  prev True = False
  prev false = True
  next = prev

class Sequence a => LeftBoundedSequence a where
  firstElem :: a

class Sequence a => RightBoundedSequence a where
  lastElem :: a

instance LeftBoundedSequence Char where
  firstElem = 'a'

instance LeftBoundedSequence Bool where
  firstElem = True

instance RightBoundedSequence Char where
  lastElem = 'z'

instance RightBoundedSequence Bool where
  lastElem = False


-- * HTML
-- ----------------------------------------------------------------------------

-- Simple (X)HTML markup.
data Attr = MkAttr String String
  deriving (Eq,Show)

data HtmlElement
  = HtmlString String                    -- Plain text.
  | HtmlTag String [Attr] HtmlElements   -- Structured markup.
  deriving (Eq, Show)

type HtmlElements = [HtmlElement]

example :: HtmlElement
example =
  HtmlTag "a" [MkAttr "href" "https://www.kuleuven.be/kuleuven/"]
    [HtmlString "KU Leuven"]

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link link text) = HtmlTag "a" [MkAttr "href" link] [HtmlString text]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [
        HtmlTag "li" [] [HtmlString "Apples"],
        HtmlTag "li" [] [HtmlString "Bananas"],
        HtmlTag "li" [] [HtmlString "Oranges"]
    ]

{-
    instance HTML String where
    toHtml s = HtmlString s
    --OR
    instance HTML [Char] where
    toHtml s = HtmlString s

    instance HTML a => HTML [a] where
    toHtml l = HtmlTag "ul" [] (map toHtml l)

    --toHtml ["Apples", "Bananas", "Oranges"]

    instance HTML a => HTML [a] where
    toHtml list = HtmlTag "ul" [] (listToHtmlList list)
        where listToHtmlList [] = []
            listToHtmlList (x:xs) = HtmlTag "li" []  [toHtml x]: listToHtmlList xs
-}

instance HTML a => HTML [a] where
  toHtml list = HtmlTag "ul" [] (listToHtmlList list)

listToHtmlList :: HTML a => [a] -> HtmlElements
listToHtmlList [] = []
listToHtmlList (x:xs) = (HtmlTag "li" [] [toHtml x]) : listToHtmlList xs

---B)-----------------------------------------

data AddressBook = Name [Email]

data Name = MkName String String
data Email = MkEmail String TypeEmail
data TypeEmail = Work | Private


printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ " " ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]



-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort [x] = [x]
selectionSort l = minn : selectionSort (delete minn l)
        where
            minn = minimum l


-- * Quicksort
-- ----------------------------------------------------------------------------

partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold fn = foldr (\x (small,big) -> if fn x then (x:small,big) else (small,x:big)) ([],[])

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter fn l = (y1,y2) 
    where
        y1 = filter fn l
        y2 = filter (not.fn) l

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC fn l = ([x | x<-l, fn x],[x | x<-l, not $ fn x])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = sortedSmall ++ x:sortedBig
    where
        (small, big) = partition (<x) xs
        sortedSmall = quicksort small
        sortedBig = quicksort big


-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

--eval (Add (Mul (Const 2) (Const 4)) (Const 3))
--11
eval :: Exp -> Int
eval (Const x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y


--Exp:   Add (Mul (Const 2) (Const 4)) (Const 3)
--Prog:  [IPush 2,IPush 4,IMul,IPush 3,IAdd]
data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush x) stack = x:stack
execute IAdd (x:y:stack) = y+x : stack
execute ISub (x:y:stack) = y-x : stack
execute IMul (x:y:stack) = y*x : stack
execute _ _ = runtimeError

run :: Prog -> Stack -> Stack
run [] s = s
run (inst:insts) stack = run insts (execute inst stack)

compile :: Exp -> Prog
compile (Const x) = [IPush x]
compile (Add x y) = compile x ++ compile y ++ [IAdd]
compile (Sub x y) = compile x ++ compile y ++ [ISub] 
compile (Mul x y) = compile x ++ compile y ++ [IMul] 


-- * Coin Change
-- ----------------------------------------------------------------------------

amountsEuro :: [Int]
amountsEuro = [1, 2, 5, 10, 20, 50, 100, 200]

changesEuro :: Int -> [[Int]]
changesEuro = changes amountsEuro

changes :: [Int] -> Int -> [[Int]]
changes _ 0 = [[]]
changes amounts total = [x:xs | x<-amounts, total-x>=0, xs<-changes (filter (>=x) amounts) (total-x) ]

amountsEuroRev :: [Int]
amountsEuroRev = reverse amountsEuro

changesEuroRev :: Int -> [[Int]]
changesEuroRev = changes amountsEuroRev

checkReverse :: Int -> Bool
checkReverse i = length (changesEuro i) == length (changesEuroRev i)