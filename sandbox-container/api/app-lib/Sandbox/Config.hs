{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sandbox.Config (Config, getConfig, excludeFiles, sandboxRoot, uiDist, apiPort, origin, publicUrl, nginxConfigPath, nginxPort) where

import qualified Data.Text as T
import qualified System.Directory as FP
import System.Environment (getEnv)

data Config = Config
  { apiPort :: Int,
    excludeFiles :: [FilePath],
    nginxConfigPath :: FilePath,
    nginxPort :: Int,
    origin :: T.Text,
    publicUrl :: T.Text,
    sandboxRoot :: FilePath,
    uiDist :: FilePath
  }
  deriving (Eq, Show)

getConfig :: IO Config
getConfig = do
  apiPort <- getEnv "HSPG_API_PORT"

  -- TODO - make configurable and support wildcards.
  -- .hspgignore or similar.
  let excludeFiles = ["dist-newstyle"]

  nginxConfigPath <- getEnv "HSPG_NGINX_CONFIG_PATH"

  _nginxPort <- getEnv "HSPG_NGINX_PORT"
  let nginxPort = read _nginxPort

  origin <- getEnv "HSPG_ORIGIN"
  publicUrl <- getEnv "HSPG_PUBLIC_URL"
  sandboxRoot <- getEnv "HSPG_SANDBOX_ROOT" >>= FP.canonicalizePath
  uiDist <- getEnv "HSPG_UI_DIST" >>= FP.canonicalizePath

  pure
    Config
      { apiPort = read apiPort,
        excludeFiles,
        nginxConfigPath,
        nginxPort,
        origin = T.pack origin,
        publicUrl = T.pack publicUrl,
        sandboxRoot,
        uiDist
      }
