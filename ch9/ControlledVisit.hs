-- file: ch9/ControlledVisit.hs
module ControlledVisit where
  
import Control.Monad (forM, liftM)
import System.Directory (getDirectoryContents, getModificationTime, Permissions(..), getPermissions)
import System.FilePath
import Control.Exception (handle, bracket, IOException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    } deriving (Eq, Ord, Show)
    
getInfo :: FilePath -> IO Info

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse order (infoPath info)
        else return [info]
        
        
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)
    
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

handleIO :: (IOException -> IO a) -> IO a -> IO a
handleIO = handle

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handleIO (\_ -> return Nothing) (Just `liftM` act)

getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  return (Info path perms size)

  