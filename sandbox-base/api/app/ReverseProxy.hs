{-# LANGUAGE QuasiQuotes #-}

module ReverseProxy (Config (Config), Upstream (Upstream, name, addr), publicUrl, upstreams, nginxConfigPath, run, renderNginxConfig) where

import qualified Data.ByteString as SD
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Exit (ExitCode (ExitFailure, ExitSuccess), exitWith)
import qualified System.Process as P
import Text.RawString.QQ (r)

data Upstream = Upstream
  { name :: T.Text,
    addr :: T.Text
  }

data Config = Config
  { publicUrl :: T.Text,
    upstreams :: [Upstream],
    nginxConfigPath :: FilePath
  }

run :: Config -> IO ()
run config = do
  let nginxConfigStr = renderNginxConfig config
  SD.writeFile (nginxConfigPath config) (encodeUtf8 nginxConfigStr)

  (_, stdout, stderr, configTest) <- P.createProcess $ P.proc "nginx" ["-c", nginxConfigPath config, "-t"]
  configTestExitCode <- P.waitForProcess configTest
  if configTestExitCode == ExitSuccess
    then do
      putStrLn "Starting nginx"
      _ <- P.createProcess $ P.proc "nginx" ["-c", nginxConfigPath config]
      pure ()
    else do
      putStrLn $ "Nginx config test failed: " <> show stdout <> show stderr
      exitWith $ ExitFailure 1

renderRoute :: Upstream -> Config -> T.Text
renderRoute upstream config = T.replace "${HSPG_PUBLIC_URL}" (name upstream) $ T.replace "${SUB_PATH}" (publicUrl config) template
  where
    template :: T.Text
    template =
      [r|
  location /${SUB_PATH}/ {
    proxy_pass http://${SUB_PATH}/;
    proxy_http_version 1.1;
    proxy_set_header Upgrade $http_upgrade;
    proxy_set_header Connection "Upgrade";
    proxy_set_header Host $host;
    proxy_pass_request_headers on;
    sub_filter_once off;
    sub_filter_types text/html;
    sub_filter "href=\"./" "href=\"${HSPG_PUBLIC_URL}/${SUB_PATH}/";
    sub_filter "src=\"./" "src=\"${HSPG_PUBLIC_URL}/${SUB_PATH}/";
    sub_filter "href=\"manifest.json\"" "href=\"${HSPG_PUBLIC_URL}/${SUB_PATH}/manifest.json\"";
    sub_filter "href=\"favicon.ico\"" "href=\"${HSPG_PUBLIC_URL}/${SUB_PATH}/favicon.ico\"";
    sub_filter "href=\"icon.svg\"" "href=\"${HSPG_PUBLIC_URL}/${SUB_PATH}/icon.svg\"";
  }
  |]

renderUpstream :: Upstream -> T.Text
renderUpstream upstream = T.replace "${ADDR}" (addr upstream) $ T.replace "${NAME}" (name upstream) template
  where
    template :: T.Text
    template =
      [r|
  upstream ${NAME} {
    least_conn;
    server ${ADDR};
  }
  |]

renderNginxConfig :: Config -> T.Text
renderNginxConfig config = T.replace "${UPSTREAMS}" upstreamsStr $ T.replace "${ROUTES}" routesStr template
  where
    upstreamsStr = T.intercalate "\n\n" $ map renderUpstream (upstreams config)
    routesStr = T.intercalate "\n\n" $ map (\u -> renderRoute u config) (upstreams config)

    template :: T.Text
    template =
      [r|
events {}

http {
  access_log /dev/stdout;
  error_log /dev/stdout;

  include /etc/nginx/mime.types;

  map $http_upgrade $connection_upgrade {
    default upgrade;
    '' close;
  }

  ${UPSTREAMS}

  server {
    listen 8080;

    ${ROUTES}
  }
}
|]
