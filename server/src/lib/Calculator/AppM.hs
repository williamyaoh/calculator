module Calculator.AppM
  ( AppM, AppConfig(..), runApp )
where

import Control.Concurrent.MVar    ( MVar )
import Control.Monad.IO.Class     ( liftIO )
import Control.Monad.Trans.Except ( ExceptT, runExceptT, throwE )
import Control.Monad.Trans.Reader ( ReaderT, runReaderT )

import Database.PostgreSQL.Simple ( Connection )

import GHC.StableName ( StableName )

import qualified Network.WebSockets as WS

import Servant ( Handler(..), ServerError )

data AppConfig = AppConfig
  { appDB      :: Connection
  , appClients :: MVar [(StableName WS.Connection, WS.Connection)]
  }

type AppM = ExceptT ServerError (ReaderT AppConfig IO)

runApp :: AppConfig -> AppM a -> Handler a
runApp cfg app = Handler $ do
  eResult <- liftIO $ flip runReaderT cfg $ runExceptT app
  case eResult of
    Left err     -> throwE err
    Right result -> pure result
