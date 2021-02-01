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
  , baseURL: "calculator.williamyaoh.com"
  , wsMethod: WSS
  , httpMethod: HTTPS
  }
