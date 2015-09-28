{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module MinionsApi.Schema where

import Data.Char
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics                 (Generic)
import Control.Monad.IO.Class       (MonadIO)
import Control.Monad.Reader         (MonadReader, asks, liftIO)
import Database.Persist             (entityIdToJSON)
import Database.Persist.Sql         (SqlPersistT, ConnectionPool, Entity(..), runSqlPool, runMigration)
import Database.Persist.TH          (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)

import MinionsApi.Config

-- Schema

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Minion
  name         String
  email        String
  password     String
  age          Int
  upset        Bool    default=false
  bananasCount Int     default=0
  deriving Show Generic
Mission
  desc      String
  location  String
  deriving Show Generic
MinionMission
  minionId  MinionId
  missionId MissionId
  UniqueMinionMission minionId missionId
  deriving Show Generic
|]
{-date      UTCTime-}

-- Codecs

-- Codecs/Minion
instance ToJSON (Entity Minion) where
  toJSON = entityIdToJSON

instance ToJSON Minion where
  toJSON Minion{..} =
    object [ "name" .= minionName
      , "email"        .= minionEmail
      , "age"          .= minionAge
      , "isUpset"      .= minionUpset
      , "bananasCount" .= minionBananasCount
    ]

instance FromJSON Minion where
  parseJSON = withObject "minion" $ \o -> do
    minionName         <- o .: "name"
    minionEmail        <- o .: "email"
    minionPassword     <- o .: "password"
    minionAge          <- o .: "age"
    minionUpset        <- o .: "upset"
    minionBananasCount <- o .: "bananasCount"
    return Minion{..}


-- Codecs/Mission
instance ToJSON (Entity Mission) where
  toJSON = entityIdToJSON
instance ToJSON Mission where
  -- An example of a field-modifier
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 7 }

instance FromJSON Mission where
  parseJSON = withObject "mission" $ \o -> do
    missionDesc     <- o .: "description"
    missionLocation <- o .: "location"
    pure Mission{..}

-- Codecs/MinionMission
instance ToJSON (Entity MinionMission) where
  toJSON = entityIdToJSON
instance ToJSON MinionMission where
  toJSON = genericToJSON defaultOptions


-- Gymnastics

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = asks getPool >>= liftIO . runSqlPool query
