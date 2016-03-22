
{-
    Outline:
    1) Tipuri monomorfice si operatii (via folduri): IList ITree
    2) Tipuri polimorfice si operatii (via folduri): List, Tree

    3) Tipul FIFO: O Coada implementata cu doua liste (stive)
    4) Operatii pe arbori: dfs, bfs, constructia unui heap.

    Studentii vor scrie tipul explicit pt fiecare functie.

-}


-- Monomorphic Lists, Trees 
data IList = INil | ICons Integer IList deriving Show
data ITree = IVoid | INode ITree Integer ITree deriving Show

-- Implementation of reverse, append on Lists
-- Implementation of conversion to/from IList to [Integer]
-- Best solution: define foldr/l first
-- Write yourself the signature of the function. How should the operator signature look like?

ifoldr :: (Integer -> a -> a) -> a -> IList -> a
ifoldr op acc INil = acc
ifoldr op acc (ICons i l) = i `op` (ifoldr op acc l)

ifoldl :: (a -> Integer -> a) -> a -> IList -> a
ifoldl op acc INil = acc
ifoldl op acc (ICons i l) = ifoldl op (acc `op` i) l

swap op = \x -> \y -> y `op` x

irev = ifoldl (swap ICons) INil
itoList = ifoldr (:) [] 
ifromList l = foldr ICons INil l

iapp = swap (ifoldr ICons)

-- Polymorphic Lists and Trees, together with reverse and append. (rely on foldr/l; Later we shall see that we need not copy-paste the code)

data List a = Nil | Cons a (List a) deriving Show
data Tree a = Void | Node (Tree a) a (Tree a)

toList = afoldr (:) [] 


-- Ignore this. Not working right.
instance Show a => Show (Tree a) where
    show t = 
        let 
            h = height t
            keyOf level (Node _ k _) = (take (fromIntegral 4*( (fromIntegral h)-level) + 1) (repeat ' ')) ++ (show k)
            op (Node Void _ Void) rest = rest
            op (Node Void _ r) rest = r:rest
            op (Node l _ Void) rest = l:rest
            op (Node l _ r) rest = l:r:rest
            bf level [] = []
            bf level trees = (foldr (++) "\n" (map (keyOf level) trees))++(bf (level+1) (foldr op [] trees))
        in bf 0 [t]


afoldr :: (b -> a -> a) -> a -> List b -> a
afoldr op acc Nil = acc
afoldr op acc (Cons i l) = i `op` (afoldr op acc l)

afoldl :: (a -> b -> a) -> a -> List b -> a
afoldl op acc Nil = acc
afoldl op acc (Cons i l) = afoldl op (acc `op` i) l

arev = afoldl (swap Cons) Nil
atoList = afoldr (:) [] 
afromList l = foldr Cons Nil l

aapp = swap (afoldr Cons)

height :: (Tree a) -> Integer
height Void = -1
height (Node l _ r) = 1 + max (height l) (height r)

-- dfs on a tree
flatten :: (Tree a) -> (List a)
flatten Void = Nil
flatten (Node t1 k t2) = (flatten t1) `aapp` (Cons k (flatten t2))

-- bfs on a tree
-- the signature bfs :: (Tree a) -> (List a) is preferable
bfs :: (Tree a) -> [a]
bfs t = 
    let keyOf (Node _ k _) = k
        op (Node Void _ Void) rest = rest
        op (Node Void _ r) rest = r:rest
        op (Node l _ Void) rest = l:rest
        op (Node l _ r) rest = l:r:rest
        bf [] = []
        bf trees = (map keyOf trees)++(bf (foldr op [] trees))
    in bf [t]


-- FIFO (O coada ca doua stive)
data FIFO a = P (List a) (List a) deriving Show

push :: a -> FIFO a -> FIFO a
push e (P l1 l2) = P (Cons e l1) l2

pull :: FIFO a -> FIFO a
pull (P l1 (Cons e l2)) = normalize $ P l1 l2

top :: FIFO a -> a
top (P l1 (Cons e l2)) = e

normalize :: FIFO a -> FIFO a
normalize (P l1 Nil) = P Nil (arev l1)
normalize (P l1 l2) = P l1 l2


-- build a heap
-- aici puteti mentiona guards: sunt intr-adevar utile, pentru a combina pattern-uri cu conditii boolene
sieve :: (Ord a) => a -> (Tree a) -> (Tree a) -> (Tree a)
sieve x l@(Node _ y _) (Node rl z rr) | z < y && z < x = Node l z (sieve x rl rr)
sieve x (Node ll y lr) r | x > y = Node (sieve x ll lr) y r
sieve x l r = Node l x r

heapify :: (Ord a) => [a] -> Tree a
heapify l = 
    let hfy i l = if i<(length l) then sieve (l !! i) (hfy (2*i+1) l) (hfy (2*i+2) l) else Void
     in hfy 0 l


t = Node (Node (Node Void 5 Void) 4 (Node Void 8 Void)) 1 (Node (Node Void 7 Void) 3 Void)
l = Node (Node Void 5 Void) 4 (Node Void 8 Void)
r = Node (Node Void 7 Void) 3 Void




