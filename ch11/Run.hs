-- file: ch11/Run.hs
import Prettify2
import Test.QuickCheck

main = do
  quickCheckWith stdArgs prop_punctuate
