{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Commands as Commands
import qualified Config
import Data.Maybe (fromJust)
import qualified Data.Text as T
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
import qualified ReverseProxy as ReverseProxy
import Servant

type SandboxApi = Fs.Api :<|> Commands.Api

sandboxApiServer :: Config.Config -> Server SandboxApi
sandboxApiServer config =
  Fs.getFsHandler config :<|> Commands.postCommandsHandler config

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

  commands <- Fs.commandsList config
  let upstreams = map mapFn (fromJust commands)
        where
          mapFn commandPath = ReverseProxy.Upstream {name = T.pack commandPath, addr = "b"}

  ReverseProxy.run
    ReverseProxy.Config
      { publicUrl = Config.publicUrl config,
        upstreams,
        nginxConfigPath = Config.nginxConfigPath config
      }

  withStdoutLogger $ \logger -> do
    putStrLn $ "Listening port: " <> show (Config.port config)
    let settings = setPort (Config.port config) $ setLogger logger defaultSettings
    runSettings settings $ app config
