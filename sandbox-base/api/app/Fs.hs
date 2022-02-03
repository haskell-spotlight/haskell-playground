{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Fs (Fs, FsChild, FsApi, listDirectoryRecursive, getFsHandler) where

import qualified Config
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import Data.Tree
import GHC.Generics (Generic)
import Servant
import qualified System.Directory as SD
import qualified System.FilePath as FP

data FsChild = File {name :: T.Text, isExecutable :: Bool} | Dir {name :: T.Text}
  deriving (Eq, Show, Generic)

instance ToJSON FsChild

type Fs = Tree FsChild

type FsApi =
  "api" :> "fs" :> "tree" :> Get '[JSON] Fs
    :<|> "api" :> "fs" :> "executables" :> Get '[JSON] Fs

listDirectoryRecursive :: FilePath -> IO (Maybe Fs)
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

          let dir = Node {rootLabel = Dir {name}, subForest}
          pure $ Just dir
        else do
          p <- SD.getPermissions filePath
          let isExecutable = SD.executable p
          let file = Node {rootLabel = File {name, isExecutable}, subForest = []}
          pure $ Just file

filterExecutables :: Fs -> Fs
filterExecutables = foldTree fn
  where
    fn :: FsChild -> [Fs] -> Fs
    fn File {name, isExecutable} subForest =
      if isExecutable
        then Node {rootLabel = File {name, isExecutable}, subForest}
        else Node {rootLabel = File {name, isExecutable}, subForest = []}
    fn Dir {name} subForest = Node {rootLabel = Dir {name}, subForest = filter sfn subForest}
      where
        sfn Node {rootLabel = File {isExecutable}} = isExecutable
        sfn Node {rootLabel = Dir {}, subForest} = not (all (null . filterExecutables) subForest)

fsAll config = do
  dir <- liftIO $ listDirectoryRecursive $ Config.sandboxRoot config
  case dir of
    Just _ -> pure $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

fsCommands config = do
  dir <- liftIO $ listDirectoryRecursive $ Config.sandboxRoot config
  case dir of
    Just _ -> pure $ filterExecutables $ fromJust dir
    _ -> throwError err500 {errBody = "Sandbox root directory not found"}

getFsHandler config = fsAll config :<|> fsCommands config
