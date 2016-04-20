-- Infinite lists

list_of_one = 1 : map (\x -> 1) list_of_one

l = map (\x -> 1) [1..]

naturals = 1 : map (+1) naturals
naturals' = [1..]
naturals'' = 1 : [ x + 1 | x <- naturals'' ] 


even_list = map (\x -> x*2) naturals'
even_list' = map (\x -> x * 2) [1..]

--fibb = tail $ scanl (+) 0 (1:fibb)
fibb = scanl (+) 1 (0:fibb)
fibb' = 1 : 1 : zipWith (+) fibb' (tail fibb')

an = map (\x -> 3*x + 2) naturals

primes = tail $ filter is_prime naturals
is_prime = \x -> null [ y | y <- tail $ take (floor (sqrt (fromIntegral x))) naturals, x `mod` y == 0 ]

-- [x, x.x, x.x.x, ...]
--let f x = x : map (x.) (f x)

--build
build g a = a : map g (build g a)
t = take 5 (build (+1) 1)

--select: tol e, list => an cu |an - a_{n+1}| < tol
select e (x:y:xs) = if (abs (x - y) < e) 
						then x 
						else select e (y:xs)

-- aprox sqrt
sq k tol = select tol (build (\an -> 1/2 * (an + k/an)) k)

-- derivative approx
-- f'(a) = (f(a+h)-f(a))/h
geom_quad h = build (/4) h
approx_deriv f a h = map (\h -> (f (a + h) - f a) / h) $ geom_quad h
deriv f a tol = select tol (approx_deriv f a 2)
