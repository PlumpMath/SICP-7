-- file: extend_exercise2_1_4.hs
cons = \ a b -> \ f -> f a b
car  = \ z -> z (\ a b -> a)
cdr  = \ z -> z (\ a b -> b)



make_interval = cons
low_bound     = car
upper_bound   = cdr

add_interval x y = make_interval ( low_bound x + low_bound y) 
                                 ( upper_bound x + upper_bound y)
mul_interval x y =
    let p1 = low_bound x * low_bound y
        p2 = low_bound x * upper_bound y
        p3 = upper_bound x * low_bound y
        p4 = upper_bound x * upper_bound y
    in 
        make_interval ( p1 `min` p2 `min` p3 `min` p4)
                      ( p1 `max` p2 `max` p3 `max` p4)


div_interval x y = mul_interval x $ make_interval (1.0 / upper_bound y) (1.0 / low_bound y)
-- for test
i1 = make_interval 1 2
i2 = make_interval 0.2 1.5

i3 = add_interval i1 i2
i4 = mul_interval i1 i2

i5 = div_interval i1 i2

-- exercise 2.7
sub_interval x y =
    let p1 = upper_bound x - low_bound y
        p2 = low_bound   x - upper_bound y
    in
        make_interval p2 p1
i6 = sub_interval i1 i2 -- [-0.5,1,8]


