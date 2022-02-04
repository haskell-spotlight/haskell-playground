{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Commands (Command, Api, initCommand, initAllCommands) where

import qualified Config
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import qualified Fs
import GHC.Generics (Generic)
import GHC.IO.Exception (ExitCode)
import Network.Wai.Handler.Warp (openFreePort)
import ReverseProxy (Upstream)
import qualified ReverseProxy
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP
import qualified System.Process as P
import Text.Show.Prettyprint as Pretty

data Command = Command {name :: T.Text, webTerminalUrl :: T.Text, pid :: T.Text, exitCode :: Maybe ExitCode}
  deriving (Eq, Show, Generic)

instance ToJSON ExitCode

instance ToJSON Command

type Api = "api" :> "exec" :> Capture "command" FilePath :> Post '[JSON] (Maybe Command)

initAllCommands :: Config.Config -> IO ()
initAllCommands config = do
  putStrLn $ "Looking commands in: " <> Config.sandboxRoot config
  _commands <- Fs.commandsList config
  let commands = fromJust _commands
  mapM_ (\command -> putStrLn $ "Found command: " <> command) commands

  maybeCsUs <- mapM (initCommand config) commands
  let csUs = Data.Maybe.catMaybes maybeCsUs
  let (_, upstreams) = unzip csUs

  ReverseProxy.run
    ReverseProxy.Config
      { publicUrl = Config.publicUrl config,
        upstreams,
        nginxConfigPath = Config.nginxConfigPath config
      }
  pure ()

initCommand :: Config.Config -> String -> IO (Maybe (Command, Upstream))
initCommand config commandRelPath = do
  putStrLn $ "Requested command: " <> commandRelPath

  if FP.isAbsolute commandRelPath
    then do
      putStrLn $ "Should be relative path: " <> commandRelPath
      pure Nothing
    else do
      let commandAbsPath = FP.combine (Config.sandboxRoot config) commandRelPath
      isCommandFound <- SD.doesFileExist commandAbsPath

      permissions <- SD.getPermissions commandAbsPath
      isDirectory <- SD.doesDirectoryExist commandAbsPath
      let isCommand = not isDirectory && SD.executable permissions

      if not (isCommandFound && isCommand)
        then do
          putStrLn $ "Command not found. Probably it's not executable. " <> commandRelPath
          pure Nothing
        else do
          putStrLn $ "Starting gotty web tty for command: " <> commandRelPath

          (gottyPort, _) <- openFreePort

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
                  commandAbsPath
                ]

          (_, _, _, p) <- P.createProcess $ P.proc "gotty" gottyArgs
          pid <- P.getPid p
          exitCode <- P.getProcessExitCode p

          let command =
                Command
                  { name = T.pack commandRelPath,
                    webTerminalUrl = Config.publicUrl config <> "/commands/" <> T.pack commandRelPath,
                    pid = T.pack $ show pid, -- XXX there should be a better way to encode pid. Please fix it if you know how.
                    exitCode = exitCode
                  }

          let upstream =
                ReverseProxy.Upstream
                  { name = T.pack commandRelPath,
                    addr = T.pack $ "127.0.0.1:" <> show gottyPort
                  }

          pure $ Just (command, upstream)
