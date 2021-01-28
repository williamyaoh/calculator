{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate ( i )

import Network.Wai.Handler.Warp ( run )
import Servant

import Text.Blaze.Html5 as Blaze hiding ( i, main )
import qualified Text.Blaze.Html5.Attributes as Blaze

import Calculator.Endpoints.HTML

type SPAEndpoint = Get '[HTML] Blaze.Html

type API =
      "app" :> "bundle" :> Raw
  :<|> "app" :> SPAEndpoint
  :<|> Get '[HTML] Blaze.Html

spa :: Handler Blaze.Html
spa = pure $! docTypeHtml $ do
  Blaze.head $ do
    title $ toHtml @String "CALC-U-LATOR 3XXX"
    Blaze.script ! Blaze.async "" ! Blaze.src "/app/bundle/index.js" $ mempty
  Blaze.body mempty

serverP :: Proxy API
serverP = Proxy

redirect :: String -> Handler a
redirect url = throwError $ err301
  { errHeaders = [("Location", [i|#{url}|])] }

server :: Server API
server =
      serveDirectoryWebApp "./bundle/"
  :<|> spa
  :<|> redirect "/app/"

main :: IO ()
main = do
  putStrLn "starting calculator server on localhost:8000..."
  run 8000 $ serve serverP server
