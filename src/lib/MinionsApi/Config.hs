{-# LANGUAGE OverloadedStrings #-}

module MinionsApi.Config
  ( Config(..)
  , Environment(..)
  , makePool      -- IO
  , lookupSetting -- IO
  , setLogger
  , defaultConfig
  ) where

import System.Environment                   (lookupEnv)
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Network.Wai                          (Middleware)
import Control.Monad.Logger                 (runNoLoggingT, runStdoutLoggingT)
import Database.Persist.Sql                 (ConnectionPool)
import Database.Persist.Postgresql          (ConnectionString, createPostgresqlPool, pgConnStr)
import Database.Persist.Sqlite              (SqliteConf(..), createSqlitePool)
import Web.Heroku.Persist.Postgresql        (postgresConf)

data Config = Config {
  getPool :: ConnectionPool,
  getEnv  :: Environment
}

data Environment =
    Development
  | Test
  | Docker
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config {
  getPool = undefined,
  getEnv  = Development
}

setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Docker      = logStdoutDev
setLogger Production  = logStdout

makePool :: Environment -> IO ConnectionPool
makePool Test        = runNoLoggingT     $ createSqlitePool (sqlDatabase $ sqliteConf Test)        (envPool Test)
makePool Development = runStdoutLoggingT $ createSqlitePool (sqlDatabase $ sqliteConf Development) (envPool Development)
makePool Production  = do
  connStr <- lookupDatabaseUrl
  runStdoutLoggingT $ createPostgresqlPool connStr (envPool Production)
makePool e           = runStdoutLoggingT $ createPostgresqlPool (psqlConf e) (envPool e)

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Docker      = 1
envPool Production  = 8

sqliteConf :: Environment -> SqliteConf
sqliteConf Test        = SqliteConf "./tmp/db-test.sqlite" 1
sqliteConf Development = SqliteConf "./tmp/db-dev.sqlite" 1
sqliteConf _ = undefined

psqlConf :: Environment -> ConnectionString
psqlConf Docker = "host=db dbname=postgres user=postgres password=postgres port=5432"
psqlConf _ = undefined

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  param <- lookupEnv env
  return $ case param of Nothing -> def
                         Just a  -> read a

lookupDatabaseUrl :: IO ConnectionString
lookupDatabaseUrl = pgConnStr <$> postgresConf 1 -- Need to remove the `1` ?
