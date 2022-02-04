{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Commands (initAllCommands)
import qualified Config
import Data.Conduit.Process.Typed (ExitCode (ExitFailure))
import qualified Fs
import GitHash (tGitInfoCwd)
import qualified GitHash as Git
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import qualified System.Directory as SD
import qualified System.Exit
import qualified System.Exit as Exit
import Text.Show.Prettyprint as Pretty

type SandboxApi = Fs.Api

sandboxApiServer :: Config.Config -> Server SandboxApi
sandboxApiServer = Fs.getFsHandler

sandboxApi :: Proxy SandboxApi
sandboxApi = Proxy

app :: Config.Config -> Application
app config = serve sandboxApi $ sandboxApiServer config

main :: IO ()
main = do
  let gitInfo = $$tGitInfoCwd
  putStrLn "Haskell Playground Sandbox"
  putStrLn $ "Revision: " <> Git.giCommitDate gitInfo <> " " <> Git.giHash gitInfo

  config <- Config.getConfig
  putStrLn $ Pretty.prettyShow config

  checkExecutablesInSystemPath

  initAllCommands config

  withStdoutLogger $ \logger -> do
    putStrLn $ "Listening port: " <> show (Config.port config)
    let settings = setPort (Config.port config) $ setLogger logger defaultSettings
    runSettings settings $ app config

checkExecutablesInSystemPath :: IO ()
checkExecutablesInSystemPath = do
  let requiredExcutables = ["nginx", "gotty"]
  putStrLn $ "Checking for required executables in system path: " <> Pretty.prettyShow requiredExcutables

  foundExecutables <- mapM (\e -> do SD.findExecutables e) requiredExcutables
  let ok = not (any null foundExecutables)

  if ok
    then do
      pure ()
    else do
      putStrLn "Some executables where not found."
      putStrLn "Required:"
      putStrLn $ Pretty.prettyShow requiredExcutables
      putStrLn "Found:"
      putStrLn $ Pretty.prettyShow foundExecutables

      _ <- Exit.exitWith $ ExitFailure 1
      pure ()
