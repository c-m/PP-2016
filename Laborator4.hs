module Laborator4 where

data NatList = NVoid | NCons Integer NatList deriving Show

nconvert :: NatList -> [Integer]
nconvert NVoid = []
nconvert (NCons i l) = i:(nconvert l)

data Option = None | Option Integer deriving (Show, Eq)

-- data Maybe a = Nothing | Just a

data Record = Short String Integer [String] | Long String Integer [String] Integer deriving Show

data RRecord = RRecord String Integer [String] Option deriving Show

-- data Rec = Ssdf Integer
-- data Record = Record ... ... Option

l = [(Short "Matei" 10 ["Andrei"]),(Short "Andrei" 4 ["Matei"]),(Long "John" 100 ["John"] 10),(Long "Mary" 100 ["John"] 10)]

olderThan :: Integer -> [Record] -> [Record]
olderThan x l = 
    let p (Short _ i _) = i >= x
        p (Long _ i _ _) = i >= x
    in filter p l

olderThan20 :: [Record] -> [Record]    
olderThan20 = olderThan 20

hasFriend :: String -> [Record] -> [Record]
hasFriend fr l = 
    let p (Short _ _ l) = elem fr l
        p (Long _ _ l _) = elem fr l
    in filter p l

friendWithMatei :: [Record] -> [Record]    
friendWithMatei = hasFriend "Matei"

hasGrade :: [Record] -> [Record]
hasGrade l = 
    let p (Short _ _ _) = False
        p (Long _ _ _ _) = True
        in filter p l

hasGrade' l = let
				p (RRecord _ _ _ None) = False
				p (RRecord _ _ _ (Option x)) = True
			  in filter p l
        
hasGrade'' l = let
				p (RRecord _ _ _ opt) = if opt == None then False else True
			  in filter p l

toRecord :: [String] -> [Integer] -> [[String]] -> [Record]
toRecord = zipWith3 (\name age friends -> Short name age friends) 

data List a = Void | Cons a (List a)

convert :: (List a) -> [a]
convert Void = []
convert (Cons e l) = e:(convert l)

foldL :: (a -> b -> a) -> a -> (List b) -> a
foldL op acc Void = acc
foldL op acc (Cons x l) = foldL op (acc `op` x) l

foldR :: (a -> b -> b) -> b -> (List a) -> b
foldR op acc Void = acc
foldR op acc (Cons x l) = x `op` foldR op acc l

mapL :: (a -> b) -> (List a) -> (List b)
mapL f = foldR (\x y->Cons (f x) y) Void

mapL' f Void = Void
mapL' f (Cons x l) = Cons (f x) $ mapL' f l

data Tree a = Nil | Node (Tree a) a (Tree a)

foldT :: (a -> b -> a -> a) -> a -> (Tree b) -> a
foldT op acc Nil = acc
foldT op acc (Node l k r) = op (foldT op acc l) k (foldT op acc r)

sumTree :: Num a => Tree a -> a
sumTree (Node l k r) = foldT (\acc1 k acc2 -> acc1 + k + acc2) 0 (Node l k r)

numsTree = Node (Node (Node Nil 1 Nil) 3 (Node Nil 4 Nil)) 5 (Node (Node Nil 6 Nil) 7 (Node Nil 8 Nil))  

mapT 	:: (a -> b) -> (Tree a) -> (Tree b)
mapT f = foldT (\x y z->Node x (f y) z) Nil 

mapT' :: (a -> b) -> (Tree a) -> (Tree b)
mapT' f Nil = Nil
mapT' f (Node l k r) = Node (mapT' f l) (f k) (mapT' f r)

zipWithT :: (a -> b -> c) -> (Tree a) -> (Tree b) -> (Tree c)
zipWithT op Nil _ = Nil
zipWithT op _ Nil = Nil
zipWithT op (Node ll lk lr) (Node rl rk rr) = Node (zipWithT op ll rl) (op lk rk) (zipWithT op lr rr)

data Pair a b = P a b deriving Show

zipLists :: (List a) -> (List b) -> (List (Pair a b))
zipLists Void Void = Void
zipLists (Cons e l) (Cons e' l') = Cons (P e e') (zipLists l l')


