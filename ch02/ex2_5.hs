-- file: ex_2_5.hs
-- if we represent 2^a * 3^b = (a,b)


expo :: Integer -> Integer -> Integer
expo base n 
    | n == 0 = 1
    | otherwise = base * expo base (n-1)

-- cons :: Integer -> Integer ->(Integer -> Integer -> Integer) -> Integer
cons a b =  \f -> f a' b'
    where a' = expo 2 a
          b' = expo 3 b
car z = z (\ a b -> a)
cdr z = z (\ a b -> b)

prints z = z (\ a b -> show a ++ "*" ++ show b)
