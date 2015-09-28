{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module MinionsApi.Service where

import Data.Int                    (Int64)
import Control.Monad               (liftM)
import Control.Monad.Reader        (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either  (EitherT, left)
import Network.Wai                 (Application)
import Database.Persist.Sql        (Entity(..), toSqlKey, fromSqlKey, get, delete, insert, selectList)
import Servant

import MinionsApi.Config
import MinionsApi.Schema

type AppM = ReaderT Config (EitherT ServantErr IO)
type MinionsAPI =
  -- Minions
       "minions"
         :> Get '[JSON] [MinionResponse]
  :<|> "minions"
         :> Capture "minionId" Int64
         :> Get '[JSON] Minion
  :<|> "minions"
         :> ReqBody '[JSON] Minion
         :> Post '[JSON] Int64
  :<|> "minions"
         :> Capture "minionId" Int64
         :> Delete '[JSON] ()
  -- Missions
  :<|> "missions"
         :> Get '[JSON] [MissionResponse]
  :<|> "missions"
         :> ReqBody '[JSON] Mission
         :> Post '[JSON] Int64
  -- Assignments
  :<|> "assign"
    :> Capture "minionId" Int64
    :> Capture "missionId" Int64
    :> Post '[JSON] Int64

type MinionResponse = Entity Minion
type MissionResponse = Entity Mission

minionsAPI :: Proxy MinionsAPI
minionsAPI = Proxy

app :: Config -> Application
app = serve minionsAPI . readerServer

readerServer :: Config -> Server MinionsAPI
readerServer cfg = enter (readerToEither cfg) server

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

server :: ServerT MinionsAPI AppM
server =
  getMinions :<|> getMinion :<|> createMinion :<|> deleteMinion :<|>
  getMissions :<|> createMission :<|>
  assign


------------------------------
-- Minion controller functions
------------------------------

getMinion :: Int64 -> AppM Minion
getMinion minionId = do
  minion <- runDb $ get (toSqlKey minionId)
  case minion of
    Just m -> return m
    Nothing -> lift $ left err404 { errBody = "minion not found" }

getMinions :: AppM [MinionResponse]
getMinions = runDb $ selectList [] []

createMinion :: Minion -> AppM Int64
createMinion = liftM fromSqlKey . runDb . insert

deleteMinion :: Int64 -> AppM ()
deleteMinion minionId = runDb $ delete (toSqlKey minionId :: MinionId)


------------------------------
-- Mission controller functions
------------------------------

getMissions :: AppM [MissionResponse]
getMissions = runDb $ selectList [] []

createMission :: Mission -> AppM Int64
createMission = liftM fromSqlKey . runDb . insert


------------------------------
-- Assignments controller functions
------------------------------

createAssignment :: MinionMission -> AppM Int64
createAssignment = liftM fromSqlKey . runDb . insert

assign :: Int64 -> Int64 -> AppM Int64
assign minionId missionId =
  let minionKey = toSqlKey minionId :: MinionId
      missionKey = toSqlKey missionId :: MissionId
   in createAssignment $ MinionMission minionKey missionKey
