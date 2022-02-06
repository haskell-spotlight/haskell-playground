{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Sandbox.Commands (Command, Api, initCommand, initAllCommands) where

import qualified Config
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes, fromJust, isJust)
import qualified Data.String as S
import qualified Data.Text as T
import Data.Text.Encoding.Base32 (encodeBase32Unpadded)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Network.Wai.Handler.Warp (openFreePort)
import ReverseProxy (Upstream)
import qualified ReverseProxy
import qualified Sandbox.FileSystem as Fs
import Servant
import qualified System.Directory as FP
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified System.Process as P
import Network.Socket.Free (getFreePort)

data Command = Command {name :: T.Text, webTerminalUrl :: T.Text, pid :: T.Text, exitCode :: Maybe ExitCode}
  deriving (Eq, Show, Generic)

instance ToJSON ExitCode

instance ToJSON Command

type Api = "api" :> "exec" :> Capture "command" FilePath :> Post '[JSON] (Maybe Command)

initAllCommands :: Config.Config -> IO ()
initAllCommands config = do
  putStrLn $ "Looking commands in: " <> Config.sandboxRoot config
  fs <- Fs.readAsTree (Config.sandboxRoot config) (Config.excludeFiles config)

  if isJust fs
    then do
      let commands = Fs.treeToList $ Fs.filterByFileKind Fs.CommandFile $ fromJust fs
      mapM_ (\command -> putStrLn $ "Found command: " <> command) commands

      maybeCsUs <- mapM (initCommand config) commands
      let (_, upstreams) = unzip $ Data.Maybe.catMaybes maybeCsUs

      ReverseProxy.run
        ReverseProxy.Config
          { publicUrl = Config.publicUrl config,
            upstreams,
            nginxConfigPath = Config.nginxConfigPath config,
            nginxPort = Config.nginxPort config
          }
      pure ()
    else do
      putStrLn "No commands where found"
      pure ()

initCommand :: Config.Config -> String -> IO (Maybe (Command, Upstream))
initCommand config commandRelPath = do
  putStrLn $ "Requested command: " <> commandRelPath

  if FP.isAbsolute commandRelPath
    then do
      putStrLn $ "Should be relative path: " <> commandRelPath
      pure Nothing
    else do
      commandAbsPath <- FP.canonicalizePath $ FP.combine (Config.sandboxRoot config) commandRelPath
      isCommandFound <- SD.doesFileExist commandAbsPath

      permissions <- SD.getPermissions commandAbsPath
      isDirectory <- SD.doesDirectoryExist commandAbsPath
      let isCommand = not isDirectory && SD.executable permissions

      if not (isCommandFound && isCommand)
        then do
          putStrLn $ "Command not found: " <> commandRelPath
          pure Nothing
        else do
          putStrLn $ "Starting gotty web tty for command: " <> commandRelPath

          gottyPort <- getFreePort

          let gottyArgs =
                [
                  -- "--title-format",
                  -- "Haskell Playground: " <> commandRelPath,
                  "--permit-write",
                  "--reconnect",
                  "--reconnect-time",
                  "600",
                  "--ws-origin",
                  T.unpack $ Config.origin config,
                  "--close-signal",
                  "9",
                  "--term",
                  "xterm",
                  commandAbsPath
                ]

          let gottyCommand = "gotty " <> S.unwords gottyArgs
          putStrLn $ "Creating new process: " <> gottyCommand
          (_, _, _, p) <- P.createProcess $ P.proc "/usr/bin/dumb-init" (["--"] <> ["bash"] <> ["-c"] <> ["set -e; export GOTTY_PORT=" <> show gottyPort <> " && gotty " <> S.unwords gottyArgs])

          pid <- P.getPid p
          exitCode <- P.getProcessExitCode p

          let commandName = T.toLower $ encodeBase32Unpadded $ T.pack commandRelPath
          let command =
                Command
                  { name = commandName,
                    webTerminalUrl = Config.publicUrl config <> "/commands/" <> commandName,
                    pid = T.pack $ show pid, -- XXX there should be a better way to encode pid. Please fix it if you know how.
                    exitCode
                  }

          let upstream =
                ReverseProxy.Upstream
                  { name = commandName,
                    addr = T.pack $ "0.0.0.0:" <> show gottyPort
                  }

          pure $ Just (command, upstream)
