{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Config (Config, getConfig, excludeFiles, sandboxRoot, port, origin, publicUrl) where

import qualified Data.Text as T
import System.Environment (getEnv)

data Config = Config
  { port :: Int,
    origin :: T.Text,
    publicUrl :: T.Text,
    sandboxRoot :: FilePath,
    excludeFiles :: [FilePath],
    nginxConf :: FilePath,
    nginxPort :: Int
  }

getConfig :: IO Config
getConfig = do
  port <- getEnv "HSPG_PORT"
  origin <- getEnv "HSPG_ORIGIN"
  publicUrl <- getEnv "HSPG_PUBLIC_URL"
  sandboxRoot <- getEnv "HSPG_SANDBOX_ROOT"
  nginxConf <- getEnv "HSPG_NGINX_CONF"

  _nginxPort <- getEnv "HSPG_NGINX_PORT"
  let nginxPort = read _nginxPort

  -- TODO - make configurable and support wildcards.
  -- .hspgignore or similar.
  let excludeFiles = ["dist-newstyle"]

  pure
    Config
      { port = read port,
        origin = T.pack origin,
        publicUrl = T.pack publicUrl,
        sandboxRoot,
        excludeFiles,
        nginxConf,
        nginxPort
      }
