
import Data.Char
import Data.List (delete, partition)
import Distribution.Simple.Utils (xargs)


-- * Sequences
-- ----------------------------------------------------------------------------

class Sequence a where
  prev :: a -> a
  next :: a -> a

instance Sequence Integer where
  prev x = x - 1
  next x = x + 1

instance Sequence Char where
  prev a = chr (ord a - 1)
  next a = chr (ord a + 1)

instance Sequence Bool where
  prev bool = not bool
  next bool = not bool

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
  lastElem = True

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
-- <a href="https://www.kuleuven.be/kuleuven/">
-- KU Leuven
-- </a>

-- HTML renderable class.
class HTML a where
  toHtml :: a -> HtmlElement

data Link =
  Link
    String  -- Link target.
    String  -- Text to show.
  deriving (Eq,Show)

instance HTML Link where
  toHtml (Link linktarget text) = HtmlTag "a" [MkAttr "href" linktarget] [HtmlString text]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" []
                [
                    HtmlTag "li" []  [HtmlString "Apples"],
                    HtmlTag "li" []  [HtmlString "Bananas"],
                    HtmlTag "li" []  [HtmlString "Oranges"]
                ]

instance HTML a => HTML [a] where
  toHtml list = HtmlTag "ul" [] (listToHtmlList list)
    where listToHtmlList [] = []
          listToHtmlList (x:xs) = HtmlTag "li" []  [toHtml x]: listToHtmlList xs


data Name = MkName
                String --first name
                String --last name

data Email = MkEmail String  TypeEmail
data TypeEmail = Work | Private
data Contact = MkContact Name [Email]

--type AddressBook = [Contact]
data AddressBook = MkAdrBook[Contact]

myAddressBook :: AddressBook
myAddressBook = MkAdrBook
                    [
                        MkContact (MkName "Andru" "Onciul") [
                            MkEmail "andru.onciul@gmail.com" Work, MkEmail "andrucristian.onciul@student.kuleuven.com" Private
                        ],
                        MkContact (MkName "client" "mana") [
                            MkEmail "private@gmail.com" Work, MkEmail "work@student.kuleuven.com" Private
                        ]
                    ]

instance HTML AddressBook where
  toHtml (MkAdrBook contacts) = HtmlTag "addressbook" [] [toHtml contacts]

instance HTML Contact where
    toHtml (MkContact name emails) = HtmlTag "contact" [] [toHtml emails]


instance HTML Email where
  toHtml (MkEmail email Work) = HtmlTag "email" [MkAttr "type" "work"] [HtmlString email]
  toHtml (MkEmail email Private) = HtmlTag "email" [MkAttr "type" "private"] [HtmlString email]



printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]




-- * Selection Sort
-- ----------------------------------------------------------------------------

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort list = s : (selectionSort newlist)
  where s = minimum list
        newlist = delete s list


-- * Quicksort
-- ----------------------------------------------------------------------------

--partitionFold (< 0) [2,3,4,-1,6,-20,0] = ([-1,-20],[2,3,4,6,0])
partitionFold :: (a -> Bool) -> [a] -> ([a],[a])
partitionFold fn = foldr funct ([],[])
  where funct = \x (ys,zs)-> if fn x then (x:ys,zs) else (ys,x:zs)

partitionFilter :: (a -> Bool) -> [a] -> ([a],[a])
partitionFilter fn l = (filter fn l, filter (not . fn) l)

partitionLC :: (a -> Bool) -> [a] -> ([a],[a])
partitionLC fn l = ( [n | n<-l, fn n], [n | n<-l, not $ fn n])

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
    where (ys,zs) = partitionFold (<x) xs


-- * Arithmetic Expressions
-- ----------------------------------------------------------------------------

data Exp = Const Int
         | Add Exp Exp
         | Sub Exp Exp
         | Mul Exp Exp
  deriving (Show, Eq)

eval :: Exp -> Int
eval (Const x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2


data Inst = IPush Int | IAdd | ISub | IMul
  deriving (Show, Eq)

type Prog  = [Inst]
type Stack = [Int]

runtimeError :: Stack
runtimeError = error "Runtime error."

execute :: Inst -> Stack -> Stack
execute (IPush x) list = x:list
execute IAdd (x:y:tail) = x+y : tail
execute ISub (x:y:tail) = y-x : tail
execute IMul (x:y:tail) = x*y : tail
execute _ list = runtimeError

--run [IAdd, ISub] [4,5,6]
--run [IAdd, ISub, IPush 7, IMul] [4,5,6,8]
run :: Prog -> Stack -> Stack
run [] s = s
run (command:coms) stack = run coms (execute command stack)

--compile (Sub (Const 1) (Mul (Const 2) (Const 3))) 
--      -> [IPush 1,IPush 2,IPush 3,IMul,ISub]
-- 1
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

--changesEuro 2 -> [[1,1],[2]]
--changesEuro 10 ->
--[[1,1,1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,2],[1,1,1,1,1,1,2,2],[1,1,1,1,1,5],
--[1,1,1,1,2,2,2],[1,1,1,2,5],[1,1,2,2,2,2],[1,2,2,5],[2,2,2,2,2],[5,5],[10]]
changes :: [Int] -> Int -> [[Int]]
changes (x:amountsEuro) tot = combine (replicate (tot `div` x) x) tot
changes [] tot = []

--[response] -> total -> responses
combine :: [Int] -> Int -> [[Int]]
combine (x:y:xs) tot = 
    if sum (x+y:xs) == tot then (x+y:xs): combine (x:y:xs) tot
    else [(x:y:xs)]


amountsEuroRev :: [Int]
amountsEuroRev = reverse amountsEuro

changesEuroRev :: Int -> [[Int]]
changesEuroRev = changes amountsEuroRev

checkReverse :: Int -> Bool
checkReverse i = length (changesEuro i) == length (changesEuroRev i)
