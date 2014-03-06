-- file: section1.hs
{-# LANGUAGE RankNTypes #-}

class Pair p where
    cons :: a -> b -> p a b
    car :: p a b -> a
    cdr :: p a b -> b

instance Pair (,) where
    cons = (,)
    car (a,_) = a
    cdr (_,b) = b

-- 2.1
data Rat a = Rat { numer :: a, denom :: a } deriving Eq

instance Show a => Show (Rat a) where
    show (Rat n d) = show n ++ "/" ++ show d

instance Integral a => Num (Rat a) where
    (Rat n1 d1) + (Rat n2 d2) = makeRat (n1 * d2 + n2 * d1) (d1 * d2)
    (Rat n1 d1) - (Rat n2 d2) = makeRat (n1 * d2 - n2 * d1) (d1 * d2)
    (Rat n1 d1) * (Rat n2 d2) = makeRat (n1 * n2) (d1 * d2)
    negate (Rat n d) = makeRat (negate n) d
    signum (Rat n d) = makeRat (signum $ n * d) 1
    abs (Rat n d) = makeRat (abs n) (abs d)
    fromInteger n = undefined

instance Integral a => Fractional (Rat a) where
    (Rat n1 d1) / (Rat n2 d2) = makeRat (n1 * d2) (d1 * n2)
    fromRational n = undefined

makeRat :: Integral a => a -> a -> Rat a
makeRat n d | d < 0 = Rat  ((-n) `div` g) ((-d) `div` g) 
            | d > 0 = Rat  (n `div` g) (d `div` g) 
            | d == 0 = error "Denominator cannot be zero"
    where g = gcd n d


-- 2.2
data Point a = Point { getX :: a, getY :: a } deriving Eq
data Line  a = Line  { start :: Point a, end :: Point a } deriving (Eq, Show)


instance Show a => Show (Point a) where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"

midPoint :: Fractional a => Line a -> Point a
midPoint (Line (Point x1 y1) (Point x2 y2) ) = Point ((x1 + x2)/2) ((y1 + y2)/2)


-- 2.5
consi a b = \f -> f (2^a) (3^b)
cari z = z (\ a b -> a)
cdri z = z (\ a b -> b)

-- 2.7 & 2.8
data Interval = Interval { lower :: Double, upper :: Double } deriving (Eq)

instance Show Interval where
    show (Interval a b) = "[" ++ show a ++ "," ++ show b ++ "]"

instance Num Interval where
    (+) = addi
    (-) = subi
    (*) = muli
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Fractional Interval where
    recip = recipi
    fromRational = undefined

addi :: Interval -> Interval -> Interval
addi (Interval a b) (Interval c d) = Interval (a + c) (b + d)


subi :: Interval -> Interval -> Interval
subi (Interval a b) (Interval c d) = Interval (a - d) (b - c)


muli :: Interval -> Interval -> Interval
muli (Interval a b) (Interval c d) = 
    let p1 = a * c
        p2 = a * d
        p3 = b * c
        p4 = b * d
    in Interval (minimum [p1,p2,p3,p4]) (maximum [p1,p2,p3,p4])

recipi :: Interval -> Interval 
recipi (Interval a b) = Interval (1/b) (1/a)

-- 2.9
-- width 
--
-- 2.10
recipiSafe :: Interval -> Interval
recipiSafe (Interval a b) = if a < 0 && b > 0
    then error "Interval in denominatoe contains zero."
    else Interval (1 / b) (1 / a)

-- 2.12
makeCenterPercent :: Double -> Double -> Interval
makeCenterPercent c p = Interval (c * (1 - p)) (c * (1 + p))

center :: Interval -> Double
center (Interval l u) = (l + u) / 2

percent :: Interval -> Double
percent (Interval l u) = (u - l) / (u + l)

-- 2.13
pair1 :: Interval -> Interval -> Interval
pair1 r1 r2 = (/) (r1 * r2) (r1 + r2)

pair2 :: Interval -> Interval -> Interval
pair2 r1 r2 = 
    let one = makeCenterPercent 1 0
    in  one / ( one / r1 + one / r2)
-- why above is not right
-- because r1 / (r1 * r2) != 1 / r2
-- r1 / r1 != 1


--2.16
--This happens because the operations in the interval arithmetic do not have the arithmetic structure of a field.
--http://en.wikipedia.org/wiki/Field_%28mathematics%29?%7B%7B%7Bqs%7D%7D%7D
