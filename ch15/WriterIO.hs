-- file: ch15/WriterIO.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad.Writer
import System.IO (IOMode(..))
import SafeHello
import MonadHandle

data Event = Open FilePath IOMode
           | Put String String
           | Close String
           | GetContents String
             deriving (Show)
             
newtype WriterIO a = W { runW :: Writer [Event] a }
    deriving (Monad, MonadWriter [Event])
    
instance MonadHandle FilePath WriterIO where
  openFile path mode = tell [Open path mode] >> return path
  hPutStr h str = tell [Put h str]
  hClose h = tell [Close h]
  hGetContents h = tell [GetContents h] >> return "fake contents"
  
runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

