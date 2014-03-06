module Ex2_module where

class Pairs p where
    cons :: a -> b -> p a b
    car  :: p a b -> a
    cdr  :: p a b -> b
    
