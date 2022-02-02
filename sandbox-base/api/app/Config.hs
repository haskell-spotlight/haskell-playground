{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Config (Config, getConfig, sandboxRoot, port, origin, publicUrl) where

import qualified Data.Text as T
import System.Environment (getEnv)

data Config = Config
  { port :: Int,
    origin :: T.Text,
    publicUrl :: T.Text,
    sandboxRoot :: FilePath
  }

getConfig :: IO Config
getConfig = do
  port <- getEnv "HSPG_PORT"
  origin <- getEnv "HSPG_ORIGIN"
  publicUrl <- getEnv "HSPG_PUBLIC_URL"
  sandboxRoot <- getEnv "HSPG_SANDBOX_ROOT"

  pure
    Config
      { port = read port,
        origin = T.pack origin,
        publicUrl = T.pack publicUrl,
        sandboxRoot
      }
