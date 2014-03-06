-- file: lambda.hs
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (succ, pred, exp, even, odd, and, or)

type Cnum a  = forall a. (a -> a) -> (a -> a)
type Cbool a = forall a. a -> a -> a

church :: Integer -> Cnum Integer
church 0 = zero
church n = \ f x -> f (church (n-1) f x)

unchurch :: Cnum Integer -> Integer
unchurch n = n (\ x -> x + 1) 0

-- Church number
zero :: Cnum a
zero = \f z -> z -- false = \ a b -> b

one, two, three :: Cnum a
one = \f -> f
two = \f -> f .f
three = \f -> f . f . f

--succ :: Cnum a -> Cnum a
succ = \ n f -> (f . n f)

--add :: Cnum a -> Cnum a -> Cnum a
add = \ m n f -> (m f . n f)
--mul :: Cnum a -> Cnum a -> Cnum a
mul = \ m n -> (m . n)

pred :: Cnum a -> Cnum a
pred n = \f z -> n (\ g h -> h (g f)) (\ u -> z) (\x -> x)

sub = \ m n -> n pred m

--exp :: Cnum a -> Cnum a -> Cnum a
exp b n = (n b)

-- Church Boolean
true, false :: Cbool a
true  = \ x y -> x
false = \ x y -> y

wrap :: Bool -> Cbool a 
wrap True  = true
wrap False = false

unwrap :: Cbool a -> Bool
unwrap b = b True False

--if_then_else :: Cbool a -> Cbool a
if_then_else = \p x y -> p x y

--not1 :: Cbool a -> Cbool a
not1 = \ f x y -> f y x

--not2 :: Cbool a -> Cbool a
not2 = \ b -> b false true

-- we should notice that \ b T F -> b

--and :: Cbool a -> Cbool a -> Cbool a
--and = \ p q -> p q p
and = \ p q -> if_then_else p q false
-- and = \ p q -> p (q true false) false

--or :: Cbool a -> Cbool a -> Cbool a
--or = \ p q -> p p q 
or = \ p q -> if_then_else p true q 

--iszero :: Cnum a -> Cbool a
iszero = \ n x y -> n (\ u -> y) x

-- f(n) = (n+3)^2
--foo :: Cnum a -> Cnum a
foo = \ n -> exp (add n three) two

-- f(n) = if n == even then true else if n == odd false
-- even :: Cnum a -> Cbool a
even = \ n -> n not1 true 

-- Turning's fixed point
-- let A = \ x y -> y (x x y)
-- theta = A A
-- N = theta F = A A F = F ( A A F) = F (theta F)
-- theta is called fixed point combinator

-- fact n = if_then_else (iszero n) one (mul n (fact (pred n))) 
--fact :: Cnum a -> Cnum a
--fact n = \ f -> (if_then_else (iszero n)) one (mul n (fact (pred n)))
--fact :: Cnum a -> Cnum a
--fact = ff fact
--ff = \ f n -> if_then_else (iszero n) one (mul (f (sub n one)) n) 
--fact zero = one
--fact n    = mul n (fact (pred n))

