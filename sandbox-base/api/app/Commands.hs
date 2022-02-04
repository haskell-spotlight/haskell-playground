{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Commands (Command, Api, postCommandsHandler) where

import qualified Config
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import Network.Wai.Handler.Warp (openFreePort)
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified System.Process as P

data Command = Command {webTerminalUrl :: T.Text, pid :: T.Text, exitCode :: Maybe ExitCode}
  deriving (Eq, Show, Generic)

instance ToJSON ExitCode

instance ToJSON Command

type Api = "api" :> "exec" :> Capture "command" FilePath :> Post '[JSON] (Maybe Command)

postCommandsHandler config commandRelPath = do
  liftIO $ putStrLn $ "Requested command: " <> commandRelPath

  if FP.isAbsolute commandRelPath
    then do
      throwError err400 {errBody = LChar8.pack $ "Command path is and absolute path, but relative to sandbox root should be provided: " <> commandRelPath}
    else do
      let commandRelPathAbsPath = FP.combine (Config.sandboxRoot config) commandRelPath
      isCommandFound <- liftIO $ SD.doesFileExist commandRelPathAbsPath

      permissions <- liftIO $ SD.getPermissions commandRelPathAbsPath
      isDirectory <- liftIO $ SD.doesDirectoryExist commandRelPathAbsPath
      let isCommand = not isDirectory && SD.executable permissions

      if isCommandFound && isCommand
        then do
          throwError err400 {errBody = LChar8.pack $ "Can't process the command request. Command file may not exist or be not commandRelPath." <> " Path: " <> commandRelPathAbsPath <> " Is commandRelPath file: " <> show isCommand}
        else do
          liftIO $ putStrLn $ "Commandsuting: " <> commandRelPathAbsPath

          (gottyPort, _) <- liftIO openFreePort

          let gottyArgs =
                [ "--permit-write",
                  "--reconnect",
                  "--reconnect-time",
                  "600",
                  "--port",
                  show gottyPort,
                  "--ws-origin",
                  T.unpack $ Config.origin config,
                  "--close-signal",
                  "9",
                  "--term",
                  "xterm",
                  commandRelPathAbsPath
                ]

          (_, _, _, p) <- liftIO $ P.createProcess $ P.proc "gotty" gottyArgs
          pid <- liftIO $ P.getPid p
          exitCode <- liftIO $ P.getProcessExitCode p

          let exec =
                Command
                  { webTerminalUrl = "abc",
                    pid = T.pack $ show pid, -- XXX there should be a better way to encode pid. Please fix it if you know how.
                    exitCode = exitCode
                  }

          pure $ Just exec
