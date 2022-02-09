{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeOperators #-}

module Sandbox.FileSystemApi (Api, api) where

import Control.Monad.IO.Class (liftIO)
import qualified Sandbox.Config as Config
import Servant
import qualified Sandbox.FileSystem as FS

type Api =
  "tree" :> QueryParams "fileKinds" FS.FileKind :> Get '[JSON] FS.Fs
    :<|> "list" :> QueryParams "fileKinds" FS.FileKind :> Get '[JSON] [FilePath]

treeHandler :: Config.Config -> [FS.FileKind] -> Handler FS.Fs
treeHandler config fileKinds = do
  dir <- liftIO $ FS.readAsTree (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Nothing -> throwError err500 {errBody = "Sandbox root directory not found"}
    Just d -> pure $ FS.filterByFileKinds fileKinds d

listHandler :: Config.Config -> [FS.FileKind] -> Handler [FilePath]
listHandler config fileKinds = do
  dir <- liftIO $ FS.readAsTree (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Nothing -> throwError err500 {errBody = "Sandbox root directory not found"}
    Just d -> pure $ FS.treeToList $ FS.filterByFileKinds fileKinds d

api :: Config.Config -> ([FS.FileKind] -> Handler FS.Fs) :<|> ([FS.FileKind] -> Handler [FilePath])
api config = treeHandler config :<|> listHandler config
