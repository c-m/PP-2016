-- Laborator 5 - clase, polimorfism

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

-- Exercitii

-- 1
data Result a = Value a | Error deriving (Eq, Show)

-- TDA-ul FIFO implementat ca 2 stive

data FIFO a = P [a] [a] deriving (Eq)

-- 2
instance Show a => Show (FIFO a) where
	show (P [] []) = "[]"
	show (P s1 []) = show $ reverse s1
	show (P [] s2) = show $ s2
	show (P s1 s2) = show $ s2 ++ reverse s1

-- operatii FIFO
push :: a -> FIFO a -> Result (FIFO a)
push e (P s1 s2) = Value (P (e:s1) s2)

pop :: FIFO a -> Result (FIFO a)
pop (P [] []) = Error
pop (P s1 []) = Value (P [] (tail $ reverse s1))
pop (P s1 s2) = Value (P s1 (tail s2))

top :: FIFO a -> Result a
top (P [] []) = Error
top (P s1 (e:s2)) = Value e

mpop :: (Eq a) => Integer -> FIFO a -> Result (FIFO a)
mpop 0 q = Error
mpop 1 q = pop q
mpop k q 
		| pop q == Error = Error
		| otherwise = mpop (k-1) q' where (Value q') = pop q


-- 3
data Expr a = Val a | Var String | Plus (Expr a) (Expr a) | Mul (Expr a) (Expr a)

-- 4
type Dictionary a = [(String, a)]

valueof :: Dictionary a -> String -> Result a
valueof [] _ = Error
valueof d s = let
				match = filter (\(x, y) -> x == s) d
			  	result = if length match > 0 then Value $ snd $ head $ match else Error
			  in result

-- 5
-- unlike the classes which we have seen so far, Evaluable does not define a set of types (i.e. a property of types), 
-- but a _RELATION_ between a type-constructor with kind * => * and a type (i.e. type constructor with type *). 
-- The relation specifies that types "t a" (containers) may be evaluated with respect to interpretations of type "Dictionary a". 
class Evaluable t a where
	-- 		context		container 	result
	eval :: Dictionary a -> t a -> Result a

-- 6
instance Evaluable Expr Integer where
	eval context (Val x) = Value x
	eval context (Var v) = valueof context v
	eval context (x `Plus` y) = case (eval context x, eval context y) of
									(Value x', Value y') -> Value (x' + y')
									_ -> Error
	eval context (x `Mul` y) = case (eval context x, eval context y) of
									(Value x', Value y') -> Value (x' * y')
									_ -> Error

instance Evaluable Expr (FIFO a) where
	eval context (Val f) = Value f
	eval context (Var v) = valueof context v
	-- Mul is concatenation
	eval context (f `Mul` f') = 
		let g (P s1 s2) (P s3 s4) = P (s1 ++ (reverse s2) ++ s3) s4
		in case (eval context f, eval context f') of
				(Value v, Value v') -> Value (v `g` v')
				_ -> Error


-- display expressions
-- ?