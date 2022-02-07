{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Config
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Sandbox.Commands (initCommands)
import qualified Sandbox.FileSystem as Fs
import Servant
import qualified System.Directory as SD
import System.Exit (ExitCode (ExitFailure))
import qualified System.Exit as Exit
import Text.Show.Prettyprint (prettyShow)

type SandboxApi = Fs.Api

sandboxApiServer :: Config.Config -> Server SandboxApi
sandboxApiServer = Fs.api

sandboxApi :: Proxy SandboxApi
sandboxApi = Proxy

app :: Config.Config -> Application
app config = serve sandboxApi $ sandboxApiServer config

main :: IO ()
main = do
  putStrLn "Haskell Playground Sandbox"
  putStrLn "Revision: TODO"

  config <- Config.getConfig
  putStrLn $ prettyShow config

  checkSystemRequirements

  _ <- initCommands config

  withStdoutLogger $ \logger -> do
    putStrLn $ "Serving API at: " <> show (Config.port config)
    let settings = setPort (Config.port config) $ setLogger logger defaultSettings
    runSettings settings $ app config

checkSystemRequirements :: IO ()
checkSystemRequirements = do
  let requiredExecutables = ["nginx", "gotty"]
  putStrLn $ "Checking for required executables in system path: " <> prettyShow requiredExecutables

  foundExecutables <- mapM SD.findExecutables requiredExecutables
  let ok = not (any null foundExecutables)

  if ok
    then do
      pure ()
    else do
      putStrLn "Some executables where not found."
      putStrLn "Executables required:"
      putStrLn $ prettyShow requiredExecutables
      putStrLn "Executables found:"
      putStrLn $ prettyShow foundExecutables

      _ <- Exit.exitWith $ ExitFailure 1
      pure ()
