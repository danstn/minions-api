{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module MinionsApi.Service where

import Data.Int                    (Int64)
import GHC.Generics                (Generic)
import Data.Aeson                  (FromJSON)
import Control.Monad               (liftM)
import Control.Monad.Reader        (ReaderT, runReaderT)
import Control.Monad.Trans.Either  (EitherT)
import Network.Wai                 (Application)
import Database.Persist.Sql        (Entity(..), fromSqlKey, insert, selectList)
import Servant

import MinionsApi.Config
import MinionsApi.Schema

type AppM = ReaderT Config (EitherT ServantErr IO)
type MinionsAPI =
       "minions"
         :> Get '[JSON] [MinionResponse]
  :<|> "minions"
         :> ReqBody '[JSON] Minion
         :> Post '[JSON] Int64

type MinionResponse = Entity Minion

minionsAPI :: Proxy MinionsAPI
minionsAPI = Proxy

app :: Config -> Application
app = serve minionsAPI . readerServer

readerServer :: Config -> Server MinionsAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT MinionsAPI AppM
server = getMinions :<|> createMinion

------------------------------
-- Minion controller functions
------------------------------

getMinions :: AppM [MinionResponse]
getMinions = runDb $ selectList [] []

createMinion :: Minion -> AppM Int64
createMinion = liftM fromSqlKey . runDb . insert

data MinionPayload = MinionPayload { cart :: String } deriving Generic
instance FromJSON MinionPayload

