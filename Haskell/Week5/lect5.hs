-- isSorted with foldmap for monoids that can be paralizable
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
--      Map each element of the structure to a monoid, and combine the results.

isSorted :: Ord a => [a] -> Bool
isSorted l = postProcess (foldMap toM l)

data M a = Sorted a a | NotSorted | SortedEmpty

toM :: a -> M a 
toM x = Sorted x x

postProcess :: Ord a => M a -> Bool
postProcess (Sorted l u) = True 
postProcess NotSorted = False 
postProcess SortedEmpty = True

instance Ord a => Semigroup (M a) where
    NotSorted <> _ = NotSorted
    _ <> NotSorted = NotSorted
    SortedEmpty <> s = s
    s <> SortedEmpty = s
    Sorted l1 u1 <> Sorted l2 u2 = 
        if u1<=l2 then Sorted l1 u2 
        else NotSorted

instance Ord a => Monoid (M a) where
    mempty = SortedEmpty

-- Now if we do a parallelized implem of foldMap, the algorithm will still be correct
