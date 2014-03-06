-- file: lambda.hs
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (succ, and, not, xor, or, even, odd, pred, mod, div)

newtype Church = Ch (forall a . (a -> a) -> (a -> a))

sh :: Church -> Integer
sh (Ch n) = n (\ x -> x + 1) 0 

{-
church :: Integer -> Church
church 0 = zero
church n = Ch (\f z -> f ( church (n-1) f z))
-}

zero :: Church
zero = Ch ( flip const )

one :: Church
one = Ch ( id )

two :: Church
two = Ch (\ f -> f . f)

three :: Church
three = Ch ( \ f -> f . f . f)

succ :: Church -> Church
succ (Ch n) = Ch (\ f -> f . n f) 

one' = succ zero
add' (Ch n) m = n succ m
add (Ch n) (Ch m) = Ch (\ f -> m f . n f)

mul (Ch n) (Ch m) = Ch (n .m)
mul' (Ch n) m     = n (add m) zero

pow (Ch n) (Ch m) = Ch (m n)
pow' n (Ch m) = m (mul n) one

four  = pow two two
five  = add four one
six   = mul two three
seven = add two five
eight = pow two three
nine  = pow three two
ten   = mul two five

dec (Ch n) = Ch ( \ f z -> fst ( n (\ (_,z) -> (z, f z)) (undefined,z) ))
dec' (Ch n) = fst (n (\ (_,z) -> (z, succ z)) (undefined,zero))
pred (Ch n) = Ch (\ f z -> n (\ g h -> h (g f)) (\ u -> z) id)

sub n (Ch m) = m dec n
sub' n (Ch m) = m dec' n
minus n (Ch m) = m pred n

-- Peano numeral system
{-
dat Nat = Zero | Succ Nat
addn Zero n = n
addn n Zero = n
addn (Succ n) m = succ (add n m)
-}

newtype CBool = Cb (forall a . a -> a -> a)

true  = Cb (\ x y -> x)
false = Cb (\ x y -> y)

csh (Cb b) = b True False

--ife :: CBool -> CBool
ife (Cb b) x y = b x y 

and :: CBool -> CBool -> CBool
and' a b = ife a b false
and (Cb a) b  = a b (Cb a)

or :: CBool -> CBool -> CBool
or (Cb a) b = a (Cb a) b
or' a b = ife a true b

not1 :: CBool -> CBool
not1 (Cb b) = b false true

not2 b = ife b false true 

iszero :: Church -> CBool 
iszero (Ch n) = n (\ u -> false) true



isequal m n = and (iszero $ minus m n) (iszero $ minus n m)
smaller m n = and (iszero $ minus m n) (not1 (iszero $ minus n m))
bigger  m n = not1 $ smaller m n

-- f(n,m) = true if even n
--        = false if odd n
even :: Church -> CBool
even (Ch n) = n not1 true

-- fixed point
fact :: Church -> Church 
fact n = ife (iszero n) one (mul n (fact (dec n)))

factorial = ff factorial
ff = \ f x -> ife (iszero x) one (mul x (f (dec x)))


-- fibonacci
fibonacci = f1 fibonacci
f1 = \ f x -> ife (smaller x two) one (add (f $ sub x one) (f $ sub x two))

-- mod
mod = f2 mod
f2 = \f n m -> ife (smaller n m) n (f (sub n m) m)

-- div
div :: Church -> Church -> Church
div n m = div' n m zero
div' = fdiv div'
fdiv = \ f n m x -> ife (smaller n m) x (f (sub n m) m (add x one))


notf f = not1 . f
-- prime 

prime n = prime' two n
prime' = fprime prime'
fprime = \ f a n -> ife (bigger (mul a a) n) true (ife (iszero $ mod n a) false (f (add a one) n))

-- Y = \ f -> f (\ x -> f (x x)) (\ x -> f (x x))
-- Y F = F (Y F)


-- data types
-- pairs
cons n m = \ f -> f n m
car = \ f -> f (\ n m -> n)
cdr = \ f -> f (\ n m -> m)
-- tuples
-- cons n1 n2 n3 .. nn = \f -> f n1 n2 .. nn
-- fst = \f -> f 
