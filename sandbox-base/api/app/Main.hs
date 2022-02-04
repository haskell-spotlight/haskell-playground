{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Config
import Exec (ExecApi, postExecHandler)
import qualified GitHash as Git
import GitHash (tGitInfoCwd)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import Fs (FsApi, getFsHandler)

type SandboxApi = FsApi :<|> ExecApi

sandboxApiServer :: Config.Config -> Server SandboxApi
sandboxApiServer config =
  getFsHandler config :<|> postExecHandler config

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

  withStdoutLogger $ \logger -> do
    putStrLn $ "Listening port: " <> show (Config.port config)
    let settings = setPort (Config.port config) $ setLogger logger defaultSettings
    runSettings settings $ app config
