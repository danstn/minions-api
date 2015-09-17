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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Minion
  name      String
  email     String
  password  String
  age       Int
  upset     Bool    default=false
  deriving Show Generic
|]

instance ToJSON (Entity Minion) where
  toJSON = entityIdToJSON
instance ToJSON Minion where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = map toLower . drop 6 }
instance FromJSON Minion where
  parseJSON = withObject "coupon" $ \o -> do
    minionName     <- o .: "name"
    minionEmail    <- o .: "email"
    minionPassword <- o .: "password"
    minionAge      <- o .: "age"
    minionUpset    <- o .: "upset"
    return Minion{..}

-- Gymnastics

runMigrations :: ConnectionPool -> IO ()
runMigrations = runSqlPool (runMigration migrateAll)

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = asks getPool >>= liftIO . runSqlPool query
