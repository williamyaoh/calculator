{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Env
  ( Env(..), env )
where

import Data.ByteString ( ByteString )

data Env = Env
  { envDBString :: ByteString
  , envPort :: Int
  }

#ifdef PROD

env :: Env
env = Env
  { envDBString = "host= port=5432 dbname=calculator user=postgres password="
  , envPort = 8000
  }

#else

env :: Env
env = Env
  { envDBString = "host=localhost port=5432 dbname=calculator user=postgres password=password"
  , envPort = 8000
  }

#endif
