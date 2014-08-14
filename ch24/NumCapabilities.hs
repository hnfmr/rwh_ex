-- file: ch24/NumCapabilities.hs
import GHC.Conc (numCapabilities)
import System.Environment (getArgs)

main = do
  args <- getArgs
  putStrLn $ "command line argument: " ++ show args
  putStrLn $ "number of cores: " ++ show numCapabilities