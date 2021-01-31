module Calculator.WebSocket where

import Prelude

import Effect.Class ( class MonadEffect, liftEffect )

import Web.Socket.WebSocket as WS

import Env ( WebSocketMethod(..), env )

-- |
-- We abstract over this so that we can change it depending
-- on dev or prod.
calculationSocket :: forall m. MonadEffect m => m WS.WebSocket
calculationSocket =
  liftEffect $ WS.create socketURL []
  where
    methodToString :: WebSocketMethod -> String
    methodToString WS = "ws"
    methodToString WSS = "wss"

    socketURL :: String
    socketURL =
      methodToString env.wsMethod <> "://" <> env.baseURL <> "/app/socket"
