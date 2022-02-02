{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson.Parser
import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Either (fromRight, isRight)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing)
import qualified Data.Text as T
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import GitHash (giCommitDate, giHash, tGitInfoCwd)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    openFreePort,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import qualified System.Directory as SD
import System.Environment (getEnv)
import System.FilePath (makeRelative)
import qualified System.FilePath as FP
import System.Process (Pid)
import qualified System.Process as P

newtype File = File {name :: T.Text}
  deriving (Eq, Show, Generic)

instance ToJSON File

data Directory = Directory {name :: T.Text, children :: [FileSystemEntry]}
  deriving (Eq, Show, Generic)

instance ToJSON Directory

data FileSystemEntry = FileEntry File | DirectoryEntry Directory
  deriving (Eq, Show, Generic)

instance ToJSON FileSystemEntry

data Exec = Exec {webTerminalUrl :: T.Text, pid :: T.Text, exitCode :: Maybe ExitCode}
  deriving (Eq, Show, Generic)

instance ToJSON ExitCode

instance ToJSON Exec

type SandboxApi =
  "api" :> "sandbox" :> "tree" :> Get '[JSON] Directory
    :<|> "api" :> "sandbox" :> "exec" :> Capture "command" FilePath :> Post '[JSON] (Maybe Exec)

listDirectoryRecursive :: FilePath -> IO (Maybe FileSystemEntry)
listDirectoryRecursive filePath = do
  isExists <- SD.doesPathExist filePath
  if not isExists
    then pure Nothing
    else do
      isDirectory <- SD.doesDirectoryExist filePath
      if isDirectory
        then do
          files <- SD.listDirectory filePath
          childrenM <- mapM (listDirectoryRecursive . FP.combine filePath) files
          let children = Data.Maybe.catMaybes childrenM

          let directoryEntry = DirectoryEntry Directory {name = T.pack $ FP.takeFileName filePath, children = children}
          pure $ Just directoryEntry
        else do
          let fileEntry = FileEntry File {name = T.pack $ FP.takeFileName filePath}
          pure $ Just fileEntry

getTreeHandler config = do
  dir <- liftIO $ listDirectoryRecursive $ sandboxRoot config
  let res = case dir of
        Just (DirectoryEntry d) -> Just d
        _ -> Nothing

  maybe
    ( throwError err500 {errBody = "Sandbox root directory not found"}
    )
    pure
    res

postExecHandler config command = do
  liftIO $ putStrLn $ "Requested command: " <> command

  if FP.isAbsolute command
    then do
      throwError err400 {errBody = LChar8.pack $ "Command is and absolute path, but relative to sandbox root should be provided: " <> command}
    else do
      let commandPath = FP.combine (sandboxRoot config) command
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
                  T.unpack $ origin config,
                  "--close-signal",
                  "9",
                  "--term",
                  "xterm",
                  commandPath
                ]

          (stdin, stdout, stderr, p) <- liftIO $ P.createProcess $ P.proc "gotty" gottyArgs
          pid <- liftIO $ P.getPid p
          exitCode <- liftIO $ P.getProcessExitCode p

          -- waiProxyTo
          let exec =
                Exec
                  { webTerminalUrl = "abc",
                    pid = T.pack $ show pid, -- XXX there should be a better way to encode pid. Please fix it if you know how.
                    exitCode = exitCode
                  }

          pure $ Just exec

sandboxApiServer :: Config -> Server SandboxApi
sandboxApiServer config =
  getTreeHandler config :<|> postExecHandler config

sandboxApi :: Proxy SandboxApi
sandboxApi = Proxy

app :: Config -> Application
app config = serve sandboxApi $ sandboxApiServer config

data Config = Config
  { port :: Int,
    origin :: T.Text,
    publicUrl :: T.Text,
    sandboxRoot :: FilePath
  }

getConfig :: IO Config
getConfig = do
  port <- getEnv "HSPG_PORT"
  origin <- getEnv "HSPG_ORIGIN"
  publicUrl <- getEnv "HSPG_PUBLIC_URL"
  sandboxRoot <- getEnv "HSPG_SANDBOX_ROOT"

  pure
    Config
      { port = read port,
        origin = T.pack origin,
        publicUrl = T.pack publicUrl,
        sandboxRoot
      }

main :: IO ()
main = do
  let gitInfo = $$tGitInfoCwd
  putStrLn "Haskell Playground Sandbox"
  putStrLn $ "Revision: " <> giCommitDate gitInfo <> " " <> giHash gitInfo

  config <- getConfig

  withStdoutLogger $ \logger -> do
    putStrLn $ "Listening port: " <> show (port config)
    let settings = setPort (port config) $ setLogger logger defaultSettings
    runSettings settings $ app config
