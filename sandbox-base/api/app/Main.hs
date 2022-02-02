{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON)
import qualified Data.Aeson.Parser
import Data.Aeson.Types (ToJSON)
import Data.Either (fromRight, isRight)
import Data.Maybe (catMaybes, fromJust, isJust, isNothing)
import qualified Data.Text as Text
import Debug.Trace (trace)
import GHC.Generics (Generic)
import GitHash (giCommitDate, giHash, tGitInfoCwd)
import Network.Wai (Application)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setPort,
  )
import Network.Wai.Logger (withStdoutLogger)
import Servant
import qualified System.Directory as SD
import System.Environment (getEnv)
import System.FilePath (makeRelative)
import qualified System.FilePath as FP

newtype File = File {name :: Text.Text}
  deriving (Eq, Show, Generic)

instance ToJSON File

data Directory = Directory {name :: Text.Text, children :: [FileSystemEntry]}
  deriving (Eq, Show, Generic)

instance ToJSON Directory

data FileSystemEntry = FileEntry File | DirectoryEntry Directory
  deriving (Eq, Show, Generic)

instance ToJSON FileSystemEntry

type SandboxAPI = "api" :> "sandbox" :> "files" :> Get '[JSON] Directory

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
          let directoryEntry = DirectoryEntry Directory {name = Text.pack $ FP.takeFileName filePath, children = children}
          pure $ Just directoryEntry
        else do
          let fileEntry = FileEntry File {name = Text.pack $ FP.takeFileName filePath}
          pure $ Just fileEntry

getSandboxApiHandler :: FilePath -> Server SandboxAPI
getSandboxApiHandler sandboxRoot = do
  dir <- liftIO $ listDirectoryRecursive sandboxRoot
  let res = case dir of
        Just (DirectoryEntry d) -> Just d
        _ -> Nothing

  maybe
    ( throwError err500 {errBody = "Sandbox root directory not found"}
    )
    pure
    res

sandboxAPI :: Proxy SandboxAPI
sandboxAPI = Proxy

app :: FilePath -> Application
app sandboxRoot = serve sandboxAPI $ getSandboxApiHandler sandboxRoot

main :: IO ()
main = do
  let port = 8080 :: Int

  let gitInfo = $$tGitInfoCwd
  putStrLn "Haskell Playground Sandbox"
  putStrLn $ "Revision: " <> giCommitDate gitInfo <> " " <> giHash gitInfo

  withStdoutLogger $ \logger -> do
    sandboxRoot <- getEnv "HASKELL_SANDBOX_ROOT"
    putStrLn $ "Sandbox root: " <> sandboxRoot

    putStrLn $ "Listening port: " <> show port
    let settings = setPort port $ setLogger logger defaultSettings
    runSettings settings $ app sandboxRoot
