{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Sandbox.FileSystem (Fs, Node, Api, FileKind (RegularFile, CommandFile), api, readAsTree, readAsList, treeToList, filterByFileKind) where

import qualified Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Tree (Tree (rootLabel))
import qualified Data.Tree as Data.List
import qualified Data.Tree as TR
import Data.Universe.Helpers (diagonal)
import GHC.Generics (Generic)
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP

data FileKind = RegularFile | CommandFile
  deriving (Eq, Show, Generic)

instance ToJSON FileKind

instance FromHttpApiData FileKind where
  parseQueryParam "command" = Right CommandFile
  parseQueryParam "regular" = Right RegularFile
  parseQueryParam "" = Right RegularFile
  parseQueryParam _ = Left "Wrong file kind provided"

determineFileKind :: FilePath -> FileKind
determineFileKind filePath
  | FP.dropExtension (FP.takeExtensions filePath) == ".x" = CommandFile
  | otherwise = RegularFile

data Node = File {name :: FilePath, kind :: FileKind} | Dir {name :: FilePath}
  deriving (Eq, Show, Generic)

instance ToJSON Node

type Fs = TR.Tree Node

type Api =
  "api" :> "fs" :> "tree" :> QueryParam "fileKind" FileKind :> Get '[JSON] Fs
    :<|> "api" :> "fs" :> "list" :> QueryParam "fileKind" FileKind :> Get '[JSON] [FilePath]

treePaths :: Tree a -> [[a]]
treePaths (TR.Node x []) = [[x]]
treePaths (TR.Node x children) = map (x :) (p : ps)
  where
    p : ps = diagonal (map treePaths children)

pathsTreeToPaths :: TR.Tree FilePath -> [FilePath]
pathsTreeToPaths tr = map mapFn (treePaths tr)
  where
    mapFn pathChunks = intercalate "/" pathChunks

treeToList :: TR.Tree Node -> [FilePath]
treeToList = pathsTreeToPaths . treeToFileNamesTree
  where
    treeToFileNamesTree :: TR.Tree Node -> TR.Tree FilePath
    treeToFileNamesTree = fmap fn where fn fsNode = name fsNode

filterByFileKind :: FileKind -> Fs -> Fs
filterByFileKind byKind = TR.foldTree foldFn
  where
    foldFn :: Node -> [Fs] -> Fs
    foldFn File {name, kind} subForest =
      if byKind == kind
        then TR.Node {rootLabel = File {name, kind}, subForest}
        else TR.Node {rootLabel = File {name, kind}, subForest = []}
    foldFn Dir {name} subForest = TR.Node {rootLabel = Dir {name}, subForest = filter sfn subForest}
      where
        sfn TR.Node {rootLabel = File {kind}} = byKind == kind
        sfn TR.Node {rootLabel = Dir {}, subForest} = not (all (null . filterByFileKind byKind) subForest)

readFsRecursive :: FilePath -> [FilePath] -> Bool -> IO (Maybe Fs)
readFsRecursive filePath excludeFiles isRoot = do
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
          childrenM <- mapM (\c -> readFsRecursive (FP.combine filePath c) excludeFiles False) files
          let subForest = Data.Maybe.catMaybes childrenM

          let dir = TR.Node {rootLabel = Dir {name}, subForest}
          pure $ Just dir
        else do
          let kind = determineFileKind name
          let file = TR.Node {rootLabel = File {name, kind}, subForest = []}
          pure $ Just file

readAsTree :: FilePath -> [FilePath] -> IO (Maybe Fs)
readAsTree root excludeFiles = readFsRecursive root excludeFiles True

readAsList :: FilePath -> [FilePath] -> IO (Maybe [FilePath])
readAsList root excludeFiles = do
  tree <- readAsTree root excludeFiles
  case tree of
    Just t -> pure $ Just (treeToList t)
    Nothing -> pure Nothing

treeHandler :: Config.Config -> Maybe FileKind -> Handler Fs
treeHandler config fileKind = do
  dir <- liftIO $ readAsTree (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Nothing -> throwError err500 {errBody = "Sandbox root directory not found"}
    Just d -> do
      let tree = case fileKind of
            Just k -> filterByFileKind k d
            Nothing -> d
      pure tree

listHandler :: Config.Config -> Maybe FileKind -> Handler [FilePath]
listHandler config fileKind = do
  dir <- liftIO $ readAsTree (Config.sandboxRoot config) (Config.excludeFiles config)
  case dir of
    Nothing -> throwError err500 {errBody = "Sandbox root directory not found"}
    Just d -> do
      let list = case fileKind of
            Just k -> treeToList $ filterByFileKind k d
            Nothing -> treeToList d
      pure list

api :: Config.Config -> (Maybe FileKind -> Handler Fs) :<|> (Maybe FileKind -> Handler [FilePath])
api config = treeHandler config :<|> listHandler config
