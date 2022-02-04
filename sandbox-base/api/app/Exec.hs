{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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

type ExecApi = "api" :> "exec" :> Capture "command" FilePath :> Post '[JSON] (Maybe Exec)

postExecHandler config executableRelPath = do
  liftIO $ putStrLn $ "Requested command: " <> executableRelPath

  if FP.isAbsolute executableRelPath
    then do
      throwError err400 {errBody = LChar8.pack $ "Executable path is and absolute path, but relative to sandbox root should be provided: " <> executableRelPath}
    else do
      let executableRelPathAbsPath = FP.combine (Config.sandboxRoot config) executableRelPath
      isExecutableFound <- liftIO $ SD.doesFileExist executableRelPathAbsPath

      permissions <- liftIO $ SD.getPermissions executableRelPathAbsPath
      isDirectory <- liftIO $ SD.doesDirectoryExist executableRelPathAbsPath
      let isExecutable = not isDirectory && SD.executable permissions

      if not isExecutableFound || not isExecutable
        then do
          throwError err400 {errBody = LChar8.pack $ "Can't process the command request. Command file may not exist or be not executableRelPath." <> " Path: " <> executableRelPathAbsPath <> " Is executableRelPath file: " <> show isExecutable}
        else do
          liftIO $ putStrLn $ "Executing: " <> executableRelPathAbsPath

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
                  executableRelPathAbsPath
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
