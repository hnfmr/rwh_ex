import Control.Monad
import System.Random

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftM2 (,) (randomRIO (1,6)) (randomRIO (1,6))

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO n = mapM (\_ -> randomIO) [1..n]

clumsyRollDice :: (Int, Int)
clumsyRollDice = (n, m)
    where
    (n, g) = randomR (1, 6) (mkStdGen 0)
    (m, _) = randomR (1, 6) g

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice gen = ((a, b), g')
    where
    (a, g) = randomR (1, 6) gen
    (b, g') = randomR (1, 6) g'
  
--    let (a, g) = randomR (1, 6) gen
--      in let (b, g') = randomR (1, 6) g
--        in ((a, b), g')
--main = do
--  (a, b) <- rollDiceIO
--  putStrLn (show a ++ ", " ++ show b)

main = do
    let ((a, b), _) = rollDice (mkStdGen 0)
      in putStrLn (show a ++ ", " ++ show b)