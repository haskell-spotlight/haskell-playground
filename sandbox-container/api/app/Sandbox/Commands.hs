{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Sandbox.Commands (Command, Api, initCommands) where

import qualified Config
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes)
import qualified Data.String as S
import qualified Data.Text as T
import Data.Text.Encoding.Base32 (encodeBase32Unpadded)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import Network.Socket.Free (getFreePort)
import ReverseProxy (Upstream)
import qualified ReverseProxy
import qualified Sandbox.FileSystem as Fs
import Servant
import qualified System.Directory as FP
import qualified System.FilePath as FP
import qualified System.Process as P

data CheckCommandRun = CheckCommandRun
  { id :: T.Text,
    startedAt :: Int,
    finishedAt :: Int,
    logsUrl :: T.Text,
    exitCode :: ExitCode
  }
  deriving (Eq, Show, Generic)

instance ToJSON CheckCommandRun

data Command
  = TermCommand {commandId :: T.Text, relPath :: FilePath, terminalUrl :: T.Text}
  | CheckCommand {commandId :: T.Text, relPath :: FilePath, terminalUrl :: T.Text, runs :: [CheckCommandRun]}
  deriving (Eq, Show, Generic)

instance ToJSON ExitCode

instance ToJSON Command

type Api = "api" :> "commands" :> Get '[JSON] [Command]

initCommands :: Config.Config -> IO [Command]
initCommands config = do
  let sandboxRoot = Config.sandboxRoot config
  putStrLn $ "Looking commands in: " <> sandboxRoot

  fs <- Fs.readAsTree (Config.sandboxRoot config) (Config.excludeFiles config)
  case fs of
    Just fs -> _initCommands fs config
    Nothing -> do
      putStrLn $ "Unable to find sandbox root " <> sandboxRoot
      pure []

_initCommands :: Fs.Fs -> Config.Config -> IO [Command]
_initCommands fs config = do
  let termCommandFiles = Fs.treeToList $ Fs.filterByFileKind Fs.TermFile fs
  let checkCommandFiles = Fs.treeToList $ Fs.filterByFileKind Fs.CheckFile fs

  let termCommands = map (mkCommand config Fs.TermFile) termCommandFiles
  let checkCommands = map (mkCommand config Fs.CheckFile) checkCommandFiles

  let commands = catMaybes $ termCommands <> checkCommands

  upstreams <- mapM (initCommand config) commands

  ReverseProxy.run
    ReverseProxy.Config
      { publicUrl = Config.publicUrl config,
        upstreams,
        nginxConfigPath = Config.nginxConfigPath config,
        nginxPort = Config.nginxPort config
      }
  pure commands

initCommand :: Config.Config -> Command -> IO Upstream
initCommand config command = do
  let commandRelPath = relPath command
  putStrLn $ "Initializing command. Id: " <> T.unpack (commandId command) <> " Path: " <> commandRelPath

  commandAbsPath <- FP.canonicalizePath $ FP.combine (Config.sandboxRoot config) commandRelPath
  gottyPort <- runGotty (T.unpack (commandId command)) commandAbsPath (T.unpack $ Config.origin config)

  pure
    ReverseProxy.Upstream
      { name = commandId command,
        addr = T.pack $ "0.0.0.0:" <> show gottyPort
      }

-- Gotty allows to share any terminal command as a web widget
-- https://github.com/sorenisanerd/gotty
runGotty :: String -> FilePath -> String -> IO Int
runGotty commandName commandAbsPath origin = do
  putStrLn $ "Starting gotty web tty for command: " <> commandName

  gottyPort <- getFreePort

  let gottyArgs =
        [ "--title-format",
          "Haskell Playground: " <> commandName,
          "--permit-write",
          "--reconnect",
          "--reconnect-time",
          "600",
          "--ws-origin",
          origin,
          "--close-signal",
          "9",
          "--term",
          "xterm",
          "bash",
          commandAbsPath
        ]

  -- dumb-init here is to avoid the Docker's PID 1 issue with zombie processes.
  let cmd = "/usr/bin/dumb-init"
  let cmdArgs = ["--"] <> ["bash"] <> ["-c"] <> ["set -ex; export GOTTY_PORT=" <> show gottyPort <> " && gotty " <> S.unwords (map (\arg -> "\"" <> arg <> "\"") gottyArgs)]

  putStrLn $ "Creating new process. Cmd: " <> cmd <> "Cmd args:" <> unwords cmdArgs
  _ <- P.createProcess $ P.proc cmd cmdArgs

  pure gottyPort

getTerminalUrl :: T.Text -> Config.Config -> T.Text
getTerminalUrl commandId config = Config.publicUrl config <> "/commands/" <> commandId

encodeCommandId :: FilePath -> T.Text
encodeCommandId relPath = T.toLower $ encodeBase32Unpadded $ T.pack relPath

mkCommand :: Config.Config -> Fs.FileKind -> FilePath -> Maybe Command
mkCommand config Fs.TermFile relPath = Just $ TermCommand {commandId, relPath, terminalUrl}
  where
    commandId = encodeCommandId relPath
    terminalUrl = getTerminalUrl commandId config
mkCommand config Fs.CheckFile relPath = Just $ CheckCommand {commandId, relPath, terminalUrl, runs = []}
  where
    commandId = encodeCommandId relPath
    terminalUrl = getTerminalUrl commandId config
mkCommand _ _ _ = Nothing
