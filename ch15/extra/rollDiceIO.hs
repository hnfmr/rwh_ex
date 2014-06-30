import Control.Monad
import System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = mapM (\_ -> randomIO) [1..n]

--main = do
--  (a, b) <- rollDiceIO
--  putStrLn (show a ++ ", " ++ show b)

main = do
    xs <- rollNDiceIO 10
    putStrLn (show xs)