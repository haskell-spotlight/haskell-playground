{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Fs (Fs, Node, Api, listDirectoryRecursive, getFsHandler, commandsList) where

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

data Node = File {name :: FilePath, isCommand :: Bool} | Dir {name :: FilePath}
  deriving (Eq, Show, Generic)

instance ToJSON Node

type Fs = TR.Tree Node

type Api =
  "api" :> "fs" :> "tree" :> Get '[JSON] Fs
    :<|> "api" :> "fs" :> "list" :> Get '[JSON] [FilePath]
    :<|> "api" :> "fs" :> "commands" :> "tree" :> Get '[JSON] Fs
    :<|> "api" :> "fs" :> "commands" :> "list" :> Get '[JSON] [FilePath]

treePaths :: Tree a -> [[a]]
treePaths (TR.Node x []) = [[x]]
treePaths (TR.Node x children) = map (x :) (p : ps)
  where
    p : ps = diagonal (map treePaths children)

pathsTreeToPaths :: TR.Tree FilePath -> [FilePath]
pathsTreeToPaths tr = map mapFn (treePaths tr)
  where
    mapFn pathChunks = intercalate "/" pathChunks

fsToPathsTree :: TR.Tree Node -> TR.Tree FilePath
fsToPathsTree = fmap fn where fn fsNode = name fsNode

fsTreeToList :: TR.Tree Node -> [FilePath]
fsTreeToList = pathsTreeToPaths . fsToPathsTree

filterCommands :: Fs -> Fs
filterCommands = TR.foldTree foldFn
  where
    foldFn :: Node -> [Fs] -> Fs
    foldFn File {name, isCommand} subForest =
      if isCommand
        then TR.Node {rootLabel = File {name, isCommand}, subForest}
        else TR.Node {rootLabel = File {name, isCommand}, subForest = []}
    foldFn Dir {name} subForest = TR.Node {rootLabel = Dir {name}, subForest = filter sfn subForest}
      where
        sfn TR.Node {rootLabel = File {isCommand}} = isCommand
        sfn TR.Node {rootLabel = Dir {}, subForest} = not (all (null . filterCommands) subForest)

listDirectoryRecursive :: FilePath -> [FilePath] -> Bool -> IO (Maybe Fs)
listDirectoryRecursive filePath excludeFiles isRoot = do
  let isExcluded = any (\toExclude -> toExclude == FP.takeFileName filePath) excludeFiles
  isExists <- SD.doesPathExist filePath

  if isExcluded || not isExists
    then pure Nothing
    else do
      let name = if isRoot then "." else FP.takeFileName filePath
      isDir <- SD.doesDirectoryExist filePath
      if isDir
        then do
          files <- SD.listDirectory filePath
          childrenM <- mapM (\c -> listDirectoryRecursive (FP.combine filePath c) excludeFiles False) files
          let subForest = Data.Maybe.catMaybes childrenM

          let dir = TR.Node {rootLabel = Dir {name}, subForest}
          pure $ Just dir
        else do
          permissions <- SD.getPermissions filePath
          let isCommand = SD.executable permissions
          let file = TR.Node {rootLabel = File {name, isCommand}, subForest = []}
          pure $ Just file

commandsList :: Config.Config -> IO (Maybe [FilePath])
commandsList config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config) True
  case dir of
    Just _ -> pure (Just $ fsTreeToList $ filterCommands $ fromJust dir)
    _ -> pure Nothing

treeHandler config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config) True
  case dir of
    Just _ -> pure $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

listHandler config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config) True
  case dir of
    Just _ -> pure $ fsTreeToList $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

commandsTreeHandler config = do
  dir <- liftIO $ listDirectoryRecursive (Config.sandboxRoot config) (Config.excludeFiles config) True
  case dir of
    Just _ -> pure $ filterCommands $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

commandsListHandler config = do
  list <- liftIO $ commandsList config
  case list of
    Just _ -> pure $ fromJust list
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

getFsHandler config =
  treeHandler config
    :<|> listHandler config
    :<|> commandsTreeHandler config
    :<|> commandsListHandler config
