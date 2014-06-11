-- file: ch11/Prettify2.hs

import Prettify
import Test.QuickCheck
import Control.Monad
         
instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM Char    arbitrary
              , liftM Text    arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]
              
prop_empty_id x =
    empty <> x == x
  &&
    x <> empty == x

prop_char c    = char c   == Char c

prop_text s    = text s   == if null then Empty else Text s

prop_line      = line     == Line

prop_double d  = double d == text (show d)