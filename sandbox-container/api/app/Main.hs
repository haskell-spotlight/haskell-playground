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
import Servant
import qualified System.Directory as SD
import System.Exit (ExitCode (ExitFailure))
import qualified System.Exit as Exit
import Text.Show.Prettyprint (prettyShow)
import qualified Sandbox.Api as Api

app :: Config.Config -> Application
app config = serve Api.api $ Api.apiServer config

main :: IO ()
main = do
  putStrLn "Haskell Playground Sandbox"
  putStrLn "Revision: TODO"

  config <- Config.getConfig
  putStrLn $ prettyShow config

  checkSystemRequirements

  _ <- initCommands config

  withStdoutLogger $ \logger -> do
    let apiPort = Config.apiPort config
    putStrLn $ "Serving API at: " <> show apiPort
    let settings = setPort apiPort $ setLogger logger defaultSettings
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
