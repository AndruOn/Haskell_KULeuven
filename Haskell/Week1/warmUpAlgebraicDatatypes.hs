
module Template where

data Name = MkName String
    deriving (Show)

data Pair = MkPair Int Int
    deriving (Show)

data Gender = Male | Female | Other
    deriving (Show)

data Person = MkPerson Name Int Gender
    deriving (Show)

data TestResult = MkTestPass Int | MkTestFail [String]
    deriving (Show)

stringToGender :: String -> Gender
stringToGender str
    | str == "Male" = Male
    | str == "Female" = Female
    | otherwise = Other

genderToString :: Gender -> String
genderToString Male = "Male"
genderToString Female = "Female"
genderToString Other = "Other"


passing :: Int -> TestResult
passing grade = MkTestPass grade

failing :: [String] -> TestResult
failing comment = MkTestFail comment

grade :: TestResult -> Int
grade (MkTestPass score) = score
grade (MkTestFail comment) = 0

comments :: TestResult -> [String]
comments (MkTestPass score) = []
comments (MkTestFail comment) = comment
