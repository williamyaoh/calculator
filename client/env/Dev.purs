module Env where

data WebSocketMethod = WS | WSS
data HTTPMethod = HTTP | HTTPS

type Env =
  { title :: String
  , baseURL :: String
  , wsMethod :: WebSocketMethod
  , httpMethod :: HTTPMethod
  }

env :: Env
env =
  { title: "CALC-U-LATOR 3000"
  , baseURL: "localhost:8000"
  , wsMethod: WS
  , httpMethod: HTTP
  }
