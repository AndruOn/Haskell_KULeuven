
module Template where

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
  toHtml (Link url text) = HtmlTag "a" 
                            [MkAttr "href" url]
                            [HtmlString text]

-- The encoding of the following unordered list as an HtmlElement
--   <ul>
--   <li>Apples</li>
--   <li>Bananas</li>
--   <li>Oranges</li>
--   </ul>
exampleUL :: HtmlElement
exampleUL = HtmlTag "ul" [] [HtmlTag "li" [] [HtmlString "Apples"], 
                            HtmlTag "li" [] [HtmlString "Bananas"], 
                            HtmlTag "li" [] [HtmlString "Oranges"]]


instance HTML a => HTML [a] where
  toHtml list = HtmlTag "ul" [] (listToHtmlList list)

listToHtmlList :: HTML a => [a] -> HtmlElements
listToHtmlList [] = []
listToHtmlList (x:xs) = (HtmlTag "li" [] [toHtml x]) : listToHtmlList xs

data AddressBook = MkAdrBook [Contact]
-- auxiliary datatypes / types used in the address book:
data Contact = Person Name [EmailAddress]
    deriving (Eq, Show)
data Name = FullName String String
    deriving (Eq, Show)
data EmailAddress = Email String EmailType
    deriving (Eq, Show)
data EmailType = Work | Private
    deriving (Eq, Show)

myAddressBook :: AddressBook
myAddressBook = MkAdrBook [Person (FullName "John" "Doe") [Email "john.doe@work.com" Work, 
                                                           Email "john.doe@private.com" Private],
                           Person (FullName "Alan" "Poe") [Email "hello.from@work.com" Work,
                                                           Email "the.other.side@private.com" Private]
                          ]


instance HTML AddressBook where
  toHtml (MkAdrBook contacts) = HtmlTag "addressbook" [] (listToHtmlList contacts)


instance HTML Contact where
  toHtml (Person name emailAddresses) = HtmlTag "contact" [] [toHtml name,
                                                              toHtml emailAddresses]


instance HTML Name where
  toHtml (FullName firstName lastName) = HtmlTag "name" [] [HtmlTag "firstname" [] [HtmlString firstName],
                                                            HtmlTag "secondname" [] [HtmlString lastName]]


instance HTML EmailAddress where
  toHtml (Email address workOrPrivate) = HtmlTag "emailaddress" [MkAttr "type" (show workOrPrivate)] [HtmlString address]


instance HTML EmailType where
  toHtml emailType = HtmlString (show emailType)

printHtmlString :: HtmlElement -> IO ()
printHtmlString = putStrLn . toHtmlString

toHtmlString :: HtmlElement -> String
toHtmlString (HtmlString s) = s ++ "<br>"
toHtmlString (HtmlTag t attrs els) =  unlines (openTag : map toHtmlString els) ++ closeTag
  where
    openTag  = '<' : t ++ " " ++ unwords (map showAttr attrs) ++ ">"
    closeTag = '<' : '/' : t  ++ ">"
    showAttr (MkAttr name value) = unwords [name, "=", '\"':value ++ "\""]