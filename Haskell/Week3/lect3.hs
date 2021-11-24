--INSERT in ordered list------------------------------------------------------
insert :: Ord a => [a] -> a -> [a]
insert [] x = [x]
insert (x:xs) a =
    if x < a
        then x:(insert xs a)
        else a:x:xs

insertOrder :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insertOrder lt [] x = [x]
insertOrder lt (x:xs) a =
    if lt x a
        then x:(insertOrder lt xs a)
        else a:x:xs

insertAsc = insertOrder (\x y -> x<y)
insertDes = insertOrder (>)

insertGeneral :: (a -> a -> Bool) -> [a] -> a -> [a]
insertGeneral cmp [] x = [x]
insertGeneral cmp (x:xs) a =
    if cmp x a
        then x:(insertGeneral cmp xs a)
        else a:x:xs

--Main> :t isort
--   isort :: (Foldable t, Ord a) => t a -> [a]
isort l = foldr (flip insert) [] l
-- or   isort l = foldr (\ x xs -> insert xs x) [] l
-- flip change the orders of the first two arguments 
-- flip :: (a -> b -> c) -> b -> a -> c 

--DATA ORD------------------------------------------------------
data MyInt = MyInt Int

instance Eq MyInt where
  MyInt n1 == MyInt n2  =  n1 == n2

instance Ord MyInt where
  MyInt n1 <= MyInt n2  =  n1 >= n2 

instance Show MyInt where
  show (MyInt n) = "MyInt " ++ show n

-- 2.a Make a datatype for pets. There are 
--     three kinds of pets: mice, dogs, elephants.

data Pet = Mouse | Dog | Elephant
    deriving Show
--DOES EXCTLY THE SAME AS:
--instance Show Pet where
--  show Mouse    = "Mouse"
--  show Dog      = "Dog"
--  show Elephant = "Elephant"


-- 2.b Write Eq, Ord and Show instances for this datatype

instance Eq Pet where
  Mouse    == Mouse    = True
  Dog      == Dog      = True
  Elephant == Elephant = True
  _        == _        = False

instance Ord Pet where
    Mouse <= _ = True 
    _ <= Elephant = True
    Dog <= Dog = True
    _ <= _ = False
    --p1 <= p2 = p1==p2 instead of: -- Dog <= Dog = True
                                    -- _ <= _ = False


-- 3.a Create a datatype for 2-d points ℕ×ℕ

data Point = MkPoint Int Int

-- 3.b Write Eq, Ord and Show instances for this datatype

instance Eq Point where
    MkPoint x1 y1 == MkPoint x2 y2 = x1==x2 && y1==y2

instance Ord Point where
    MkPoint x1 y1 <= MkPoint x2 y2 = x1 < x2 || (x1==x2 && y1 <= y2)

instance Show Point where
  show (MkPoint x y) = "(" ++ show x ++ "," ++ show y ++ ")"

  -- 4.a Create a dataytype for representing abstract syntax trees
---    of arithmetic expressions.
--     An expression can be either:
--     -- a constant (an integer)
--     -- a variable (with some name)
--     -- an addition of two subexpressions

data SyntaxTree = Const Int | Var String | Add SyntaxTree SyntaxTree

-- 4.b Write a Show instance for this datatype.à)çà

--Add (Const 3) (Add (Var "x") (Add (Const 3) (Const 4)))
--Add (Const 3) (Add (Var "x") (Const 3))
instance Show  SyntaxTree where
    show (Const x) = show x
    show (Var x) = x --ATTENTION HERE I DONT NEED A SHOW BC STRING IS ALREADY A STRING (if i add it it will create " " around it)
    show (Add t1 t2) = show t1 ++ " + " ++ show t2

-- 4.c Write an evaluation function.

--eval (\str -> if str=="x" then 5 else -1) (Add (Const 3) (Add (Var "x") (Const 3)))
eval :: (String -> Int) -> SyntaxTree -> Int
eval env (Const n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Var x) = env x

-- HOMEWORK:

--   Use alternative environment representation
--     [(String,Int)]
--     [("x",5)]

-- 5. Make a datatype to represent a trie datastructure.


