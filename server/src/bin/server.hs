{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PolyKinds #-}

module Main where

import Data.String.Interpolate ( i )
import Data.Foldable ( traverse_ )
import Data.ByteString ( ByteString )
import Data.Text ( Text )

import Control.Monad ( forever, void )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.MVar
import Control.Exception ( catch )

import GHC.StableName ( StableName, makeStableName )

import Network.HTTP.Types ( status400 )
import qualified Network.WebSockets as WS
import Network.Wai ( responseLBS )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Handler.WebSockets
import Servant

import Text.Blaze.Html5 as Blaze hiding ( i, main )
import qualified Text.Blaze.Html5.Attributes as Blaze

import Calculator.Endpoints.HTML

type SPAEndpoint = Get '[HTML] Blaze.Html

type API =
       "app" :> "bundle" :> Raw
  :<|> "app" :> "socket" :> Raw
  :<|> "app" :> SPAEndpoint
  :<|> Get '[HTML] Blaze.Html

type Connections = [(StableName WS.Connection, WS.Connection)]

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

-- |
-- Accept incoming websocket connections and store them so that
-- we can push new calculations to them. We don't actually need to
-- send any data here; that's the responsibility of the rest of the
-- API.
socketServer :: MVar Connections -> Tagged m Application
socketServer conns = Tagged $
  websocketsOr WS.defaultConnectionOptions socketApp backupApp
  where
    -- We *could* receive all incoming requests through WS as well, but
    -- if this were a real app, it would be more useful to have the API
    -- accessible without having to open a socket anyways.
    socketApp :: WS.ServerApp
    socketApp pending = do
      conn <- WS.acceptRequest pending
      cid <- makeStableName conn
      modifyMVar_ conns (pure . (:) (cid, conn))
      catch (forever $ pullData conn) $
        \e -> do
          _ <- pure (e :: WS.ConnectionException)
          modifyMVar_ conns (pure . filter (\(cid', _) -> cid' /= cid))

    -- We want to keep receiving messages so that we eventually see the
    -- close request.
    pullData :: WS.Connection -> IO ()
    pullData = void . WS.receiveDataMessage

    backupApp :: Application
    backupApp _ respond = respond $
      responseLBS status400 [] "Not a WebSocket request"

server :: MVar Connections -> Server API
server clients =
       serveDirectoryWebApp "./bundle/"
  :<|> socketServer clients
  :<|> spa
  :<|> redirect "/app/"

keepAlive :: MVar Connections -> IO ()
keepAlive conns = go 0
  where
    go :: Int -> IO ()
    go n = do
      threadDelay 30000000  -- every 30s
      withMVar conns $ \conns -> do
        putStrLn [i|current \# of open clients: #{length conns}|]
        flip traverse_ conns $ \(_, conn) -> do
          WS.sendPing conn ([i|#{n}|] :: ByteString)
          WS.sendTextData conn ([i|hello!|] :: Text)
      go (n+1)

main :: IO ()
main = do
  putStrLn "starting calculator server on localhost:8000..."
  clients <- newMVar []
  void $ forkIO $ keepAlive clients
  run 8000 $ serve serverP $ server clients
