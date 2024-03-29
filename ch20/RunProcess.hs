-- file: ch20/RunProcess.hs
-- {-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE FlexibleInstances #-}

module RunProcess where

import System.Process
import Control.Concurrent
import Control.Concurrent.MVar
-- import Control.OldException(evaluate)
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
                         forkProcess (child fds stdinread stdoutwrite))
                         
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
           return $ CommandResult {cmdOutput = hGetContents stdouthdl,
                                   getExitStatus = waitfunc}
        -- Define what happens in the child process
        where child closefds stdinread stdoutwrite =
                do -- Copy our pipes over the regular stdin/stdout FDs
                  dupTo stdinread stdInput
                  dupTo stdoutwrite stdOutput

                  -- Now close the original pipe FDs
                  closeFd stdinread
                  closeFd stdoutwrite

                  -- Close all the open FDs we inherited from the parent
                  mapM_ (\fd -> catch (closeFd fd) (\_ -> return ())) closefds

                  -- Start the program
                  executeFile cmd True args Nothing

{- | An instance of 'CommandLike' for an external command. The String is passed
 to a shell for evaluation and invocation. -}
instance CommandLike String where
    invoke cmd closefds input =
        do -- Use the shell given by the environment variables SHELL,
           -- if any. Otherwise, use /bin/sh
           esh <- getEnv "SHELL"
           let sh = case esh of
                        Nothing -> "/bin/sh"
                        Just x -> x
           invoke (sh, ["-c", cmd]) closefds input

-- Add FDs to the list of FDs that must be closed post-fork in a child
addCloseFDs :: CloseFDs -> [Fd] -> IO ()
addCloseFDs closefds newfds =
    modifyMVar_ closefds (\oldfds -> return $ oldfds ++ newfds)

removeCloseFDs :: CloseFDs -> [Fd] -> IO ()
removeCloseFDs closefds removethem =
    modifyMVar_ closefds (\fdlist -> return $ procfdlist fdlist removethem)

    where
    procfdlist fdlist [] = fdlist
    procfdlist fdlist (x:xs) = procfdlist (removefd fdlist x) xs

    -- We want to remove only the first occurance of any given fd
    removefd [] _ = []
    removefd (x:xs) fd
        | fd == x = xs
        | otherwise = x : removefd xs fd
