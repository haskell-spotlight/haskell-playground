{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Exec (Exec, ExecApi, postExecHandler) where

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

data Exec = Exec {webTerminalUrl :: T.Text, pid :: T.Text, exitCode :: Maybe ExitCode}
  deriving (Eq, Show, Generic)

instance ToJSON ExitCode

instance ToJSON Exec

type ExecApi = "api" :> "sandbox" :> "exec" :> Capture "command" FilePath :> Post '[JSON] (Maybe Exec)

postExecHandler config command = do
  liftIO $ putStrLn $ "Requested command: " <> command

  if FP.isAbsolute command
    then do
      throwError err400 {errBody = LChar8.pack $ "Command is and absolute path, but relative to sandbox root should be provided: " <> command}
    else do
      let commandPath = FP.combine (Config.sandboxRoot config) command
      isCommandFound <- liftIO $ SD.doesFileExist commandPath

      permissions <- liftIO $ SD.getPermissions commandPath
      let isExecutable = SD.executable permissions
      isDirectory <- liftIO $ SD.doesDirectoryExist commandPath
      let isExecutableFile = not isDirectory && isExecutable

      if not isCommandFound || not isExecutableFile
        then do
          throwError err400 {errBody = LChar8.pack $ "Can't process the command request. Command file may not exist or be not executable." <> " Path: " <> commandPath <> " Is executable file: " <> show isExecutableFile}
        else do
          liftIO $ putStrLn $ "Executing: " <> commandPath

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
                  commandPath
                ]

          (_, _, _, p) <- liftIO $ P.createProcess $ P.proc "gotty" gottyArgs
          pid <- liftIO $ P.getPid p
          exitCode <- liftIO $ P.getProcessExitCode p

          let exec =
                Exec
                  { webTerminalUrl = "abc",
                    pid = T.pack $ show pid, -- XXX there should be a better way to encode pid. Please fix it if you know how.
                    exitCode = exitCode
                  }

          pure $ Just exec
