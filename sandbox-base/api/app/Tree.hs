{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Tree (TreeApi, File, Directory, FileSystemEntry, listDirectoryRecursive, getTreeHandler) where

import qualified Config
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP
import Control.Monad.IO.Class (liftIO)

newtype File = File {name :: T.Text}
  deriving (Eq, Show, Generic)

instance ToJSON File

data Directory = Directory {name :: T.Text, children :: [FileSystemEntry]}
  deriving (Eq, Show, Generic)

instance ToJSON Directory

data FileSystemEntry = FileEntry File | DirectoryEntry Directory
  deriving (Eq, Show, Generic)

instance ToJSON FileSystemEntry

type TreeApi = "api" :> "sandbox" :> "tree" :> Get '[JSON] Directory

listDirectoryRecursive :: FilePath -> IO (Maybe FileSystemEntry)
listDirectoryRecursive filePath = do
  isExists <- SD.doesPathExist filePath
  if not isExists
    then pure Nothing
    else do
      isDirectory <- SD.doesDirectoryExist filePath
      if isDirectory
        then do
          files <- SD.listDirectory filePath
          childrenM <- mapM (listDirectoryRecursive . FP.combine filePath) files
          let children = Data.Maybe.catMaybes childrenM

          let directoryEntry = DirectoryEntry Directory {name = T.pack $ FP.takeFileName filePath, children = children}
          pure $ Just directoryEntry
        else do
          let fileEntry = FileEntry File {name = T.pack $ FP.takeFileName filePath}
          pure $ Just fileEntry

getTreeHandler config = do
  dir <- liftIO $ listDirectoryRecursive $ Config.sandboxRoot config
  let res = case dir of
        Just (DirectoryEntry d) -> Just d
        _ -> Nothing

  maybe
    ( throwError err500 {errBody = "Sandbox root directory not found"}
    )
    pure
    res
