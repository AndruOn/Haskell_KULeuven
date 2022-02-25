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
  (==) MyTrue MyTrue = True
  (==) MyFalse MyFalse = True
  (==) _ _ = False

instance Eq Exp where
  (==) (Const b1) (Const b2) = b1==b2
  (==) (And b1 b2) (And b3 b4) = b1==b3 && b2==b4
  (==) (Or b1 b2) (Or b3 b4) = b1==b3 && b2==b4
  (==) _ _ = False

-- -------------------------------------------------
-- Printing
-- -------------------------------------------------

instance Show MyBool where
  show MyTrue = show True
  show MyFalse = show False

instance Show Exp where
  show (Const b) = show b
  show (And b1 b2) = show b1 ++ " && " ++ show b2
  show (Or b1 b2) = show b1 ++ " || " ++ show b2

-- -------------------------------------------------
-- Evaluating
-- -------------------------------------------------

class Evaluatable a where
  eval :: a -> Bool

instance Evaluatable MyBool where
  eval MyTrue = True
  eval MyFalse = False

instance Evaluatable Exp where
  eval (Const b) = eval b
  eval (And x y) = eval x && eval y
  eval (Or x y) = eval x || eval y