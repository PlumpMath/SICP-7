-- file: section2.hs


data Cons a = Nil | Cons a (Cons a) deriving (Eq)

isNil :: Cons a -> Bool
isNil Nil = True
isNil _   = False

instance Show a => Show (Cons a) where
    show x = "(" ++ str x ++ ")"
        where
            str Nil          = ""
            str (Cons x Nil) = show x
            str (Cons x y)   = show x ++ " " ++ str y

car :: Cons a -> a
car Nil = error "Can't take the car of Nil"
car (Cons x _) = x

cdr :: Cons a -> Cons a
cdr Nil = error "Can't take the cdr of Nil"
cdr (Cons _ y) = y

lengthi :: Num a => Cons t -> a
lengthi Nil = 0
lengthi (Cons x xs) = 1 + lengthi xs



list :: [a] -> Cons a
list [] = Nil
list (x:xs) = Cons x (list xs)


foldri f zero Nil = zero
foldri f zero (Cons x y) = f x (foldri f zero y)

foldli f zero Nil = zero
foldli f zero (Cons x y) = foldli f (f zero x) y

tolist :: Cons a -> [a]
tolist = foldri (:) []

-- 2.17
lastPair :: Cons a -> Cons a
lastPair p@(Cons x Nil) = p
lastPair (Cons x y) = lastPair y

-- 2.18
reversei :: Cons a -> Cons a
reversei list = iter list Nil
    where iter Nil result = result
          iter (Cons x y) result = iter y (Cons x result)

cs :: a -> Cons a -> Cons a
cs a list = Cons a list
reverselist = foldli (flip cs) Nil

-- 2.19
{-
count_change amount = cc amount 5
    where cc amount kind_of_coins
                | amount == 0 = 1
                | amount < 0 || kind_of_coins == 0 = 0
                | otherwise = cc amount (kind_of_coins - 1) 
                                + cc (amount - first_demo kind_of_coins) kind_of_coins 
          first_demo num | num == 5 = 1
                         | num == 4 = 5
                         | num == 3 = 10
                         | num == 2 = 25
                         | num == 1 = 50
-}
count_change amount = cc amount us_coins
    where
        cc amount coins
            | amount == 0 = 1
            | amount < 0 || isNil coins = 0
            | otherwise = cc amount (cdr coins) + cc (amount - car coins) coins
        us_coins = list [1,5,10,25,50]


-- 2.20
-- samePairty
samePairty [x] = [x]
samePairty (x:xs) = x : filter ( \ y -> odd x == odd y) xs

-- mapping over lists
mapi f Nil = Nil
mapi f (Cons x xs) = Cons (f x) (mapi f xs)

-- 2.21 - 2.22
--squareList = foldli (\xs x -> Cons (x*x) xs) Nil
squareList = foldri (\x xs -> Cons (x*x) xs) Nil

-- 2.23
--foreach f (Cons x Nil) = f x
--foreach f (Cons x xs)  = (f x) && foreach f xs
foreach :: Monad m => (a -> m b) -> Cons a -> m ()
foreach f Nil = return ()
foreach f (Cons x xs) = do f x
                           foreach f xs


-- tree data
data Tree a = Leaf a | Branch (Cons (Tree a)) deriving (Eq)

instance Show a => Show (Tree a) where
    show (Leaf x) = show x
    show (Branch xs) = show xs

lengtht (Leaf x) = 1
lengtht (Branch xs) = lengthi xs

countLeafs (Leaf x) = 1
countLeafs (Branch Nil) = 0
countLeafs (Branch (Cons x xs)) = countLeafs x + countLeafs (Branch xs)


-- test
t1 = Branch $ list [Leaf 1, Leaf 2]
t2 = Leaf 3
t3 = Leaf 4
t  = Branch $ list [t1, t2, t3]

cars (Leaf x)     = Leaf x
cars (Branch xs)  = car xs

cdrs (Leaf x)     = error "cant take the cdrs of the leaf"
cdrs (Branch xs)  = cdr xs

branchFromList [x] = Leaf x
branchFromList  xs = Branch $ makeBranch xs
    where
        makeBranch [] = Nil
        makeBranch (x:xs) = Cons (Leaf x) (makeBranch xs)

treeFromList = Branch . makeTree
    where
        makeTree []  = Nil
        makeTree (x:xs) = Cons (branchFromList x) (makeTree xs)
