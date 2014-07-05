-- file: ch15/extra/diceState.hs
import Control.Monad
import Control.Monad.Trans.State
import System.Random

type GeneratorState = State StdGen

rollDie :: GeneratorState Int
rollDie = do generator <- get
             let (value, newGenerator) = randomR (1,6) generator
             put newGenerator
             return value

rollDice :: GeneratorState (Int, Int)
rollDice = liftM2 (,) rollDie rollDie
