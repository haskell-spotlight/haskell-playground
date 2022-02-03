{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Fs (Fs, FsNode, FsApi, listDirectoryRecursive, getFsHandler) where

import qualified Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
import Data.Tree (Tree (rootLabel))
import qualified Data.Tree as Data.List
import qualified Data.Tree as TR
import Data.Universe.Helpers (diagonal)
import GHC.Generics (Generic)
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP

data FsNode = File {name :: FilePath, isExecutable :: Bool} | Dir {name :: FilePath}
  deriving (Eq, Show, Generic)

instance ToJSON FsNode

type Fs = TR.Tree FsNode

type FsApi =
  "api" :> "fs" :> "tree" :> Get '[JSON] Fs
    :<|> "api" :> "fs" :> "list" :> Get '[JSON] [FilePath]
    :<|> "api" :> "fs" :> "executables" :> "tree" :> Get '[JSON] Fs
    :<|> "api" :> "fs" :> "executables" :> "list" :> Get '[JSON] [FilePath]

treePaths :: Tree a -> [[a]]
treePaths (TR.Node x []) = [[x]]
treePaths (TR.Node x children) = map (x :) (p : ps)
  where
    p : ps = diagonal (map treePaths children)

pathsTreeToPaths :: TR.Tree FilePath -> [FilePath]
pathsTreeToPaths tr = map mapFn (treePaths tr)
  where
    mapFn pathChunks = intercalate "/" (["."] <> pathChunks)

fsToPathsTree :: TR.Tree FsNode -> TR.Tree FilePath
fsToPathsTree = fmap fn where fn fsNode = name fsNode

fsTreeToList :: TR.Tree FsNode -> [FilePath]
fsTreeToList = pathsTreeToPaths . fsToPathsTree

filterExecutables :: Fs -> Fs
filterExecutables = TR.foldTree foldFn
  where
    foldFn :: FsNode -> [Fs] -> Fs
    foldFn File {name, isExecutable} subForest =
      if isExecutable
        then TR.Node {rootLabel = File {name, isExecutable}, subForest}
        else TR.Node {rootLabel = File {name, isExecutable}, subForest = []}
    foldFn Dir {name} subForest = TR.Node {rootLabel = Dir {name}, subForest = filter sfn subForest}
      where
        sfn TR.Node {rootLabel = File {isExecutable}} = isExecutable
        sfn TR.Node {rootLabel = Dir {}, subForest} = not (all (null . filterExecutables) subForest)

listDirectoryRecursive :: FilePath -> [FilePath] -> IO (Maybe Fs)
listDirectoryRecursive filePath excludeFiles = do
  let isExcluded = any (\toExclude -> toExclude == FP.takeFileName filePath) excludeFiles
  isExists <- SD.doesPathExist filePath

  if isExcluded || not isExists
    then pure Nothing
    else do
      let name = FP.takeFileName filePath
      isDir <- SD.doesDirectoryExist filePath
      if isDir
        then do
          files <- SD.listDirectory filePath
          childrenM <- mapM (\c -> listDirectoryRecursive (FP.combine filePath c) excludeFiles) files
          let subForest = Data.Maybe.catMaybes childrenM

          let dir = TR.Node {rootLabel = Dir {name}, subForest}
          pure $ Just dir
        else do
          p <- SD.getPermissions filePath
          let isExecutable = SD.executable p
          let file = TR.Node {rootLabel = File {name, isExecutable}, subForest = []}
          pure $ Just file

fsTree config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Just _ -> pure $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

fsList config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Just _ -> pure $ fsTreeToList $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

fsExecutablesTree config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Just _ -> pure $ filterExecutables $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

fsExecutablesList config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Just _ -> pure $ fsTreeToList $ filterExecutables $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

getFsHandler config =
  fsTree config
    :<|> fsList config
    :<|> fsExecutablesTree config
    :<|> fsExecutablesList config
