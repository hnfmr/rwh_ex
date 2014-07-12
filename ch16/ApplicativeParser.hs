-- file: ch16/ApplicativeParsec.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ApplicativeParsec
    (
      module Control.Applicative
    , module Text.ParserCombinators.Parsec
    ) where
    
import Control.Applicative
import Control.Monad (MonadPlus(..), ap)
-- Hide a few names that are provided by Applicative.
--import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec
import Numeric(readHex)

-- The Applicative instance for every Monad looks like this.
--instance Applicative (GenParser s a) where
--    pure  = return
--    (<*>) = ap
    
-- The Alternative instance for every MonadPlus looks like this.
--instance Alternative (GenParser s a) where
--    empty = mzero
--    (<|>) = mplus
    
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
    where hexify a b = toEnum . fst . head . readHex $ [a,b]