{-
-what is the difference between the findfixpoint and improve method?
 just as the name of two procedure, findfixpoint is the abstraction of the find fix point method.
 and improve is just an abstraction of the improve method, so the findfixpoint's level is higher than the improve method
-
- -}
{-findfixpoint ::  (Fractional a, Ord a) => (a -> a) -> a -> a-}
{-findfixpoint f guess = if goodenough guess next-}
                        {-then next-}
                        {-else findfixpoint f next-}
                        {-where next = f guess-}
                              {-goodenough a b = abs ((a - b) / a)  < 0.0001-}
findfixpoint f guess = improve close f guess
    where close a  = abs ((a - f a) / a)  < 0.0001


averageDump f x = ( f x + x ) / 2

sqrtMe x = findfixpoint f x
    where f = averageDump (\y -> x / y)


{-improve ::  (t -> Bool) -> (t -> t) -> t -> t-}
improve close update guess = fixp guess
    where fixp guess = if close guess
                           then guess
                           else fixp $ update guess

{-sqrtTwo ::  (Fractional t, Ord t) => t -> t-}
sqrtTwo x = improve close update 1.0
    where close y = abs (y * y - x) < 0.0001
          update y = (x / y + y) / 2

{-goldPoint ::  Double-}
goldPoint = improve close update 1.0
    where close x = abs (x * x - x - 1) < 0.001
          update x = (1 + 1 / x)

