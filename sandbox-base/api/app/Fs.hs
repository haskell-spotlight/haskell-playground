{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Fs (FsApi, File, Dir, listDirectoryRecursive, getFsHandler) where

import qualified Config
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (ToJSON)
import Data.List (foldl')
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import Data.Tree
import qualified Data.Tree as Tree
import GHC.Generics (Generic)
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP

data File = File {name :: T.Text, isDir :: Bool, isCommand :: Bool}
  deriving (Eq, Show, Generic)

instance ToJSON File

type Dir = Tree File

type FsApi = "api" :> "fs" :> Get '[JSON] Dir

listDirectoryRecursive :: FilePath -> IO (Maybe Dir)
listDirectoryRecursive filePath = do
  isExists <- SD.doesPathExist filePath
  if not isExists
    then pure Nothing
    else do
      let name = T.pack $ FP.takeFileName filePath
      isDir <- SD.doesDirectoryExist filePath
      if isDir
        then do
          files <- SD.listDirectory filePath
          childrenM <- mapM (listDirectoryRecursive . FP.combine filePath) files
          let subForest = Data.Maybe.catMaybes childrenM

          let dir = Node {rootLabel = File {name, isDir, isCommand = False}, subForest}
          pure $ Just dir
        else do
          p <- SD.getPermissions filePath
          let isCommand = SD.executable p
          let file = Node {rootLabel = File {name, isCommand, isDir}, subForest = []}
          pure $ Just file

getFsHandler config = do
  dir <- liftIO $ listDirectoryRecursive $ Config.sandboxRoot config
  case dir of
    Just _ -> pure $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

-- getExecutableChildren :: Dir -> [FileSystemEntry]
-- getExecutableChildren dir = filter fn (children dir)
--   where
--     fn (FileEntry file) = isExecutable file
--     fn _ = False

-- filterExecutables :: Directory -> Directory
-- filterExecutables dir = foldl' f (Directory { name = name dir, children = []} ) dir where
--   f parDir currDir =
