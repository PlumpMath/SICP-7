-- file: ex2_1.hs
import Prelude hiding (gcd)
-- what is a rational
-- define it use procedure
-- make_rat = \ n d -> (\f -> f n d) 
make_rat n d =  \f -> f n' d'
    where [n',d'] = simply [n,d]

-- sameSymb (a,b) = all (>0) [a,b] || all (<0) [a,b]
sameSymb xs = or [all (>0) xs, all (<0) xs]

simply xs@[n,d]
    | sameSymb xs = map (`div` g) abs_xs
    | otherwise   = map (`div` g) [-an,ad]
    where abs_xs@[an,ad] = map abs xs
          g = gcd an ad

gcd a b
    | b == 0 = a
    | a < b  = gcd b a
    | otherwise = gcd b (a `mod` b)
    
{-
simply f = \ a b -> (\f -> f a' b')
                where a' = a `div` g
                      b' = b `div` g
                      g  = gcd a b
-}


 {-   where gcd a b
            | b == 0 = a
            | a < b  = gcd b a
            | otherwise = gcd b (a `mod` b)
          g = gcd n d
-}
numer = \ z -> z (\ n d -> n)
denom = \ z -> z (\ n d -> d)



-- there is no type for numer z, so this can not go through
-- print_rat = \ z -> (show (numer z)) ++ " / " ++ (show (denom z))
