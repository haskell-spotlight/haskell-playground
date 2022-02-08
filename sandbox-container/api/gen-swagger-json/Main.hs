module Main where

import Sandbox.Api (writeSwaggerJson)

main :: IO ()
main = do
  putStrLn "Generating swagger.json"
  writeSwaggerJson

