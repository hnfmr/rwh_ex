-- file: ch6/eqclasses.hs

class BasicEq a where
    isEqual :: a -> a -> Bool
    
    