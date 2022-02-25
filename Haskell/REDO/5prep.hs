import Control.Applicative -- backwards compatibility
import Control.Monad

-- * Functors
-- ----------------------------------------------------------------------------

data Identity a = Identity a
  deriving (Eq, Ord, Show)

data Pair a b = Pair a b
  deriving (Eq, Ord, Show)

data Unit a = Unit

instance Eq (Unit a) where
  Unit == Unit = True

instance Show (Unit a) where
  show Unit = "Unit"

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor (Pair a) where
   fmap f (Pair a b) = Pair a (f b)

instance Functor Unit where
  fmap f Unit = Unit


-- * Monoids and Foldables
-- ----------------------------------------------------------------------------

data Sign = Pos | Zero | Neg | Any deriving (Show, Eq)

instance Semigroup Sign where
  Zero <> s     =  s
  s    <> Zero  =  s
  Any <> _     =  Any
  _    <> Any  =  Any
  Pos  <> Pos   =  Pos
  Neg  <> Neg   =  Neg
  _    <> _     =  Any

instance Monoid Sign where
  mempty = Zero

data Cases a = Split (Cases a) (Cases a) | Case a
  deriving Show

instance Foldable Cases where
--foldMap :: Monoid m => (a -> m) -> t a -> m 
  foldMap fLeaf (Case a) = fLeaf a
  foldMap fLeaf (Split c1 c2) = foldMap fLeaf c1 <> foldMap fLeaf c2
  

signCases :: Cases Int -> Sign
signCases cases = foldMap toSign cases

toSign :: Int -> Sign
toSign x 
    | x==0 = Zero
    | x<0 = Neg
    | x>0 = Pos