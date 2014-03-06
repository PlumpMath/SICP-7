-- file: ex2_2.hs

-- make_segment
-- start_segment
-- end_segment
-- make_point
-- x_point
-- y_point
-- midpoint_segment

-- point (x,y)
{-
make_point = \x y -> \f -> f x y
x_point = \ z -> z (\ a b -> a)
y_point = \ z -> z (\ a b -> b)

make_segment  = \ p1 p2 -> \f p1 p2
start_segment = \ z -> z (\ a b -> a)
end_segment   = \ z -> z (\ a b -> b)
-}
make_pair = \x y -> \f -> f x y
first     = \ z -> z (\ a b -> a)
second    = \ z -> z (\ a b -> b)

-- make_point :: Integer -> Integer -> (Integer -> Integer -> Integer)
make_point = make_pair
x_point    = first
y_point    = second

make_segment  = make_pair
start_segment = first
end_segment   = second

--print_point p = "(" ++ show ((x_point p) :: Integer) ++ "," ++ show ((y_point p) :: Integer) ++ ")"

print_point p = p (\ a b -> show a ++ "," ++ show b)

print_segment s = (print_point $ start_segment s) ++ " -- " ++ (print_point $ end_segment s)
