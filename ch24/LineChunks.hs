-- file: ch24/LineChunks.hs
module LineChunks
    (
      chunkedReadWith
    ) where
    
import Control.Exception (bracket, finally)
import Control.Monad (forM, liftM)
import Control.Parallel.Strategies (NFData, rnf)
import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as LB
import GHC.Conc (numCapabilities)
import System.IO

data ChunkSpec = CS {
       chunkOffset :: !Int64
     , chunkLength :: !Int64
     } deriving (Eq, Show)
     
withChunks :: (NFData a) =>
              (FilePath -> IO [ChunkSpec])
           -> ([LB.ByteString] -> a)
           -> FilePath
           -> IO a