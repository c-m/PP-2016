module Lab5 where

import Laborator4

concatL :: (List a) -> (List a) -> (List a)
concatL Void l = l
concatL (Cons e l) l' = Cons e $ concatL l l'

flattenTreePre :: (Tree a) -> (List a)
flattenTreePre Nil = Void
flattenTreePre (Node left key right) = (flattenTreePre left) `concatL` (Cons key Void) `concatL` (flattenTreePre right)

flattenTreeIn :: (Tree a) -> (List a)	
flattenTreeIn Nil = Void
flattenTreeIn (Node left key right) = (Cons key Void) `concatL` (flattenTreeIn left) `concatL` (flattenTreeIn right)

flattenTreePost :: (Tree a) -> (List a)
flattenTreePost Nil = Void
flattenTreePost (Node left key right) = (flattenTreePost left) `concatL` (flattenTreePost right) `concatL` (Cons key Void)

showlist Void = ""
showlist (Cons a Void) = show a
showlist (Cons a l) = (show a) ++ ":" ++ (showlist l)

instance Show a => Show (List a) where
	show l = "{" ++ (showlist l) ++ "}" 

showtree Nil = ""
showtree (Node l k r) = "Node( " ++ (showtree l) ++ " " ++ (show k) ++ " "  ++ (showtree r) ++ ")"
instance Show a => Show (Tree a) where
	show = showtree

areEqual :: (Eq a) => (List a) -> (List a) -> Bool
areEqual Void Void = True
areEqual l Void = False
areEqual Void l = False
areEqual (Cons a l1) (Cons b l2) = a == b && areEqual l1 l2

instance (Eq a) => Eq (List a) where
	(==) = areEqual

areEqualT :: (Eq a) => (Tree a) -> (Tree a) -> Bool
areEqualT Nil Nil = True
areEqualT Nil r = False
areEqualT l Nil = False
areEqualT (Node l1 k1 r1) (Node l2 k2 r2) = (k1 == k2) && areEqualT l1 l2 && areEqualT r1 r2

instance (Eq a) => Eq (Tree a) where
	(==) = areEqualT


filterL :: (a -> Bool) -> (List a) -> (List a)
filterL f Void = Void
filterL f (Cons x xs) = if f x then Cons x $ filterL f xs 
						else filterL f xs

quicksort :: (Ord a) => List a -> List a  
quicksort Void = Void
quicksort (Cons x xs) = (quicksort lesser) `concatL` (Cons x Void) `concatL` (quicksort greater)
						where
							lesser = filterL (<x) xs
							greater = filterL (>=x) xs

binsearch :: (Ord a) => a -> (Tree a) -> (Maybe a)
binsearch _ Nil = Nothing
binsearch x (Node l k r) 
						| x == k = (Just k)
						| x > k = binsearch x r
						| x < k = binsearch x l

instance Functor List where
	fmap = mapL'

instance Functor Tree where
	fmap = mapT'