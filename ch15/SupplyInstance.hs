-- file: ch15/SupplyInstance.hs
newtype Reader e a = R { runReader :: e -> a }

instance Monad (Reader e) where
    return a = R $ \_ -> a
    m >>= k = R $ \r -> runReader (k (runReader m r)) r