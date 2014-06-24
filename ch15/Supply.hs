-- file: ch15/Supply.hs

module Supply
    (
      Supply
    , next
    , runSupply
    ) where
    
import Control.Monad.State

newtype Supply s a = S (State [s] a)

runSupply :: Supply s a -> [s] -> (a, [s])

next :: Supply s (Maybe s)