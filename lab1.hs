-- Laborator 1 PP

{-- Exercitii -}

-- 1a. Factorial

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = (*) n $ factorial (n-1)

-- 1b. Factorial tail-recursive

factorial' :: Integer -> Integer
factorial' n = let 
				  facAux 0 result = result
				  facAux n result = facAux (n-1) (n * result) 
			   in facAux n 1

-- 2a. nth Fibbonacci 

fibo 0 = 1
fibo 1 = 1
fibo n = fibo (n-1) + fibo (n-2)

-- 2b. nth Fibbonacci tail-recursive

fibTail n = if n == 0 then 1
			else let
					fibAux 0 result prev = result
					fibAux n result prev = fibAux (n-1) (result + prev) result 
				 in fibAux n 1 0

-- 3. Mergesort

mergesort [] = []
mergesort [x] = [x]
mergesort l = let
				 merge l [] = l
				 merge [] l = l
				 merge l@(h:t) l'@(h':t') = if h < h' then h:(merge t l') else h':(merge l t')
				 half = (length l) `div` 2
			  in merge (mergesort $ take half l) (mergesort $ drop half l)


-- 4. Insertion Sort

insSort n [] = [n]
insSort n (x:xs) = if n < x then n:x:xs
				   else x : (insSort n xs)

insertionSort [] = []
insertionSort (x:xs) = insSort x $ insertionSort xs

-- 5. Quicksort

quicksort [] = []
quicksort (x:xs) = let
					  lesser = quicksort [a | a <- xs, a <= x]
					  greater = quicksort [a | a <- xs, a > x]
				   in lesser ++ [x] ++ greater

