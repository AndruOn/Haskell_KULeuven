
import Data.Char
import Data.List (delete)


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
quicksort = error "Not implemented"