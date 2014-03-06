-- file: ex2_3.hs

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

print_point p = "(" ++ show ((x_point p) :: Integer) ++ "," ++ show ((y_point p) :: Integer) ++ ")"

print_segment s = (print_point $ start_segment s) ++ " -- " ++ (print_point $ end_segment s)

-- rectangle w h
make_rectangle = make_pair
width          = first
height         = second


make_rectangle_point p1 p2 = make_pair p1 p2 
width_point  r             = r (\ p1 p2 -> abs(x_point p1 - x_point p2))
height_point r             = r (\ p1 p2 -> abs(y_point p1 - y_point p2))


perimeter r = (+) (width r :: Integer) (height r :: Integer) * 2
area      r = (*) (width r :: Integer) (height r :: Integer)

