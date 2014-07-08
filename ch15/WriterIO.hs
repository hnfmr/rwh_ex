-- file: ch15/WriterIO.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Writer
import System.IO (IOMode(..))

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)
             
newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])
    
runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

