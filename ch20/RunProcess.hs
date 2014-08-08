-- file: ch20/RunProcess.hs
{-# OPTIONS_GHC -fglasgow-exts #-}

module RunProcess where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
import Control.OldException(evaluate)
import System.Posix.Directory
import System.Directory(setCurrentDirectory)

import System.IO
import System.Exit
import Text.Regex
import System.Posix.Process
import System.Posix.IO
import System.Posix.Types
import Data.List
import System.Posix.Env(getEnv)

{- | The type for running external commands. The first part of the tupe is the
program name. The list represents the command-line parameters to pass to the
command. -}
type SysCommand = (String, [String])

{- | The result of running any command -}
data CommandResult = CommandResult {
    cmdOutput :: IO String,               -- ^ IO action that yields the output
    getExitStatus :: IO ProcessStatus     -- ^ IO action that yields exit result
}

{- | The type for handling global lists of FDs to always close in the clients -}
type CloseFDs = MVar [Fd]

{- | Class representing anything that is a runnable command -}
class CommandLike a where
    {- | Given the command and a String representing input,
         invokes the command. Returns a String
         representing the output of the command. -}
    invoke :: a -> CloseFDs -> String -> IO CommandResult
    
-- Support for running system commands
instance CommandLike SysCommand where
    invoke (cmd, args) closefds input =
        do -- Create two pipes: one to handle stdin and the other
           -- to handle stdout. We do not redirect stderr in this program.
           (stdinread, stdinwrite) <- createPipe
           (stdoutread, stdoutwrite) <- createPipe
           
           -- We add the parent FDs to this list because we always need to close
           -- them in the clients
           addCloseFDs closefds [stdinwrite, stdoutread]
           
           -- Now, grab the closed FDs list and fork the child.
           childPID <- withMVar closefds (\fds ->
                         forkProcess (child fds stdinread stdoutwrite)
                         
           -- Now, on the parent, close the client-side FDs.
           closeFd stdinread
           closeFd stdoutwrite
          
           -- Write the input to the command.
           stdinhdl <- fdToHandle stdinwrite
           forkIO $ do hPutStr stdinhdl input
                       hClose stdinhdl
                      
           -- Prepare to receive output from the command
           stdouthdl <- fdToHandle stdoutread

           -- Set up the function to call when ready to wait for the child to
           -- exit
           let waitfunc =
               do status <- getProcessStatus True False childPID
                   case status of
                       Nothing -> fail $ "Error: Nothing from getProcessStatus"
                       Just ps -> do removeCloseFDs closefds
                                         [stdinwrite, stdoutread]
                                     return ps
           return $ CommandResult {cmdOutput = hGetContents sdouthdl,
                                  getExitStatus = waitfunc}
















          