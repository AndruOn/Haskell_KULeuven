module Template where

data MyBool = MyTrue
            | MyFalse

data Exp = Const MyBool
         | And Exp Exp
         | Or Exp Exp

-- -------------------------------------------------
-- Equality Checking
-- -------------------------------------------------

instance Eq MyBool where
  MyTrue == MyTrue = True
  MyFalse == MyFalse = True 
  _ == _ = False

instance Eq Exp where
  And x1 y1 == And x2 y2 = x1==x2 && y1==y2
  Or x1 y1 == Or x2 y2 = x1==x2 && y1==y2
  Const x1 == Const x2 = x1==x2
  _ ==_ = False 

-- -------------------------------------------------
-- Printing
-- -------------------------------------------------

instance Show MyBool where
  show MyTrue = "True"
  show MyFalse = "False"

instance Show Exp where
  show (And x y) = show x ++ " && " ++ show y
  show (Or x y) = show x ++ " || " ++ show y
  show (Const x) = show x

-- -------------------------------------------------
-- Evaluating
-- -------------------------------------------------

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
  eval MyTrue = True
  eval MyFalse = False

instance Evaluatable Exp where
  eval (And x1 x2) = eval x1 && eval x2
  eval (Or x1 x2) = eval x1 || eval x2
  eval (Const x) = eval x

