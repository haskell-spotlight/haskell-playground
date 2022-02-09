{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedLists #-}

module Sandbox.FileSystem (Fs, Node, FileKind (TermFile, CheckFile, ViewFile, AnyFile), readAsTree, readAsList, treeToList, filterByFileKinds) where

import Data.Aeson
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Tree (Tree (rootLabel))
import qualified Data.Tree as Data.List
import qualified Data.Tree as TR
import Data.Universe.Helpers (diagonal)
import GHC.Generics (Generic)
import Servant ( FromHttpApiData(parseQueryParam) )
import qualified System.Directory as SD
import qualified System.FilePath as FP

data FileKind = TermFile | CheckFile | ViewFile | AnyFile
  deriving (Eq, Show, Generic)

instance ToJSON FileKind

instance ToParamSchema FileKind

instance ToSchema FileKind

instance FromHttpApiData FileKind where
  parseQueryParam "TermFile" = Right TermFile
  parseQueryParam "CheckFile" = Right CheckFile
  parseQueryParam "ViewFile" = Right ViewFile
  parseQueryParam "AnyFile" = Right AnyFile
  parseQueryParam _ = Left "Wrong file kind provided"

determineFileKind :: FilePath -> FileKind
determineFileKind filePath
  | FP.takeExtension (FP.dropExtension (FP.takeExtensions filePath)) == ".term" = TermFile
  | FP.takeExtension (FP.dropExtension (FP.takeExtensions filePath)) == ".check" = CheckFile
  | FP.takeExtension (FP.dropExtension (FP.takeExtensions filePath)) == ".view" = ViewFile
  | otherwise = AnyFile

data Node = File {name :: FilePath, kind :: FileKind} | Dir {name :: FilePath}
  deriving (Eq, Show, Generic)

instance ToJSON Node where
  toJSON =
    genericToJSON
      defaultOptions
        { sumEncoding = ObjectWithSingleField
        }

instance ToJSON (TR.Tree Node) where
  toJSON =
    genericToJSON
      defaultOptions
        { sumEncoding = ObjectWithSingleField
        }

instance ToSchema Node

instance ToSchema (TR.Tree Node)

type Fs = TR.Tree Node

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

filterByFileKinds :: [FileKind] -> Fs -> Fs
filterByFileKinds byKinds = TR.foldTree foldFn
  where
    foldFn :: Node -> [Fs] -> Fs
    foldFn File {name, kind} subForest =
      if kind `elem` byKinds
        then TR.Node {rootLabel = File {name, kind}, subForest}
        else TR.Node {rootLabel = File {name, kind}, subForest = []}
    foldFn Dir {name} subForest = TR.Node {rootLabel = Dir {name}, subForest = filter sfn subForest}
      where
        sfn TR.Node {rootLabel = File {kind}} = kind `elem` byKinds
        sfn TR.Node {rootLabel = Dir {}, subForest} = not (all (null . filterByFileKinds byKinds) subForest)

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
