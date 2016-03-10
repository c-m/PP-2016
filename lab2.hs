import Data.Char (ord, chr)
-- Laborator 2 PP


-- quick sort

quicksort [] = []
quicksort (x:xs) = let
					  lesser = quicksort [a | a <- xs, a <= x]
					  greater = quicksort [a | a <- xs, a > x]
				   in lesser ++ [x] ++ greater

-- functii anonime

lambda = \x y -> x*y
lambda' = \l1 -> \l2 -> l2 ++ l1

-- Curry vs uncurry (si de ce nu conteaza in Haskell)

-- definiti o functie care prefixeaza [1,2,3] la o lista primita ca parametru, ca o inchidere functionala

pref l1 l2 = l1 ++ l2
pref_close = pref [1,2,3]


-- Definiti o functie de ordin superior, care primeste o functie si un numar, si aplica de doua ori functia pe numarul respectiv

f = \g x -> (g.g) x
-- apel: f (\x->x*2) 2


-- Definiti o functie care primeste un operator binar, si intoarce acelasi operator in care ordinea parametrilor a fost inversata (e.g. 1/3 → 3/1)

invert_param = \binary_op -> \x y -> y `binary_op` x
-- apel: invert_param (/) 1 3


-- map, foldl, foldr, filter, zipWith, (.)

my_map f [] = []
my_map f (x:xs) = (f x) : my_map f xs

my_foldl f acc [] = acc
my_foldl f acc (x:xs) = my_foldl f (f acc x) xs

my_rev = my_foldl (flip (:)) []

my_foldr f acc [] = acc
my_foldr f acc (x:xs) = f x (my_foldr f acc xs)

-- exercitii: sum, prod, size
my_filter f [] = []
my_filter f (x:xs) = if f x then x : my_filter f xs else my_filter f xs


my_zipWith _ [] _ = []
my_zipWith _ _ [] = []
my_zipWith f (x:xs) (y:ys) = f x y : my_zipWith f xs ys

compose_f = \f g -> f g

-- ex: powerset
{--
P(S) = {a ∪ b | a ∈ {{}, {s}}, b ∈ P(S')}
     = {b | b ∈ P(S')} ∪ {{s} ∪ b | b ∈ P(S')}
     = P(S') ∪ {{s} ∪ b | b ∈ P(S')}
     = let p = P(S') in p ∪ {{s} ∪ b | b ∈ p}
--}
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)


-- Implementati folosind foldl sau foldr: map, filter

map' f = foldr ((:).f) []

filter' f = foldr (\x y -> if f x then x : y else y) []


-- ex: transpose matrix

transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)


-- ex: ROT13 encryption
-- ord :: Char -> Int , chr :: Int -> Char
rot13 :: [Char] -> [Char]
rot13 message = map crypt message
	where 
		crypt c = chr (aux c + ord 'a')
			where
				aux c = (ord c - ord 'a' + 13) `mod` 26
