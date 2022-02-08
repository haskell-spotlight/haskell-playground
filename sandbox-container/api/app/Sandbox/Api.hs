{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Sandbox.Api (Api, api, apiServer) where

import qualified Config
import Control.Lens
import Data.Swagger (HasBasePath (basePath), HasDescription (description), HasInfo (info), HasTitle (title), HasVersion (version), Swagger)
import qualified Sandbox.FileSystem as Fs
import Servant
import Servant.Swagger (HasSwagger (toSwagger))
import Servant.Swagger.UI
  ( SwaggerSchemaUI,
    swaggerSchemaUIServer,
  )

type SandboxApi = "fs" :> Fs.Api

sandboxApi :: Proxy SandboxApi
sandboxApi = Proxy

sandboxSwagger :: Swagger
sandboxSwagger =
  toSwagger sandboxApi
    & basePath ?~ "/api"
    & info . title .~ "Haskell Playground Sandbox API"
    & info . version .~ "1.0"
    & info . description ?~ "https://github.com/haskell-spotlight/haskell-playground"

type Api =
  SandboxApi
    :<|> SwaggerSchemaUI "_" "swagger.json" -- TODO - There should be a way to serve it on "/api" root instead of "_".

apiServer :: Config.Config -> Server Api
apiServer config = Fs.api config :<|> swaggerSchemaUIServer sandboxSwagger

api :: Proxy Api
api = Proxy
