module Main where

import Development.Shake
import Development.Shake.FilePath

outputDir :: FilePath
outputDir = "output/"

data BuildEnv = Dev | Prod

main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = outputDir, shakeVerbosity = Verbose } $ do
  want [ "bundle-dev" ]

  phony "bundle-dev" $ do
    need [ "client-dev"
         , outputDir </> "server"
         ]

  phony "bundle-prod" $ do
    need [ "client-prod"
         , outputDir </> "server"
         ]

  outputDir </> "server" %> \file -> do
    need ["./server/server.cabal"]

    command_ [Cwd "./server/"] "cabal"
      [ "v2-install"
      , "server-exe"
      , "--installdir=."
      , "--install-method=copy"
      , "--overwrite-policy=always"
      ]
    copyFileChanged "./server/server-exe" file

  phony "client-dev" $ produceJSBundle Dev
  phony "client-prod" $ produceJSBundle Prod

  outputDir </> "bundle/main.css" %> \file -> do
    copyFileChanged "./client/css/main.css" file

produceJSBundle :: BuildEnv -> Action ()
produceJSBundle env = do
    need ["./client/spago.dhall", outputDir </> "bundle/main.css"]

    command_ [Cwd "./client/"] "spago" ["bundle-app", "-p", "./env/" </> toEnvFile env]
    copyFileChanged "./client/index.js" $ outputDir </> "bundle/index.js"
  where toEnvFile :: BuildEnv -> FilePath
        toEnvFile Dev = "Dev.purs"
        toEnvFile Prod = "Prod.purs"
