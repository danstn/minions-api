module SpecHelper (withTestApp) where

import Test.Hspec
import Test.Hspec.Wai
import Network.Wai

import MinionsApi.Service      (app)
import MinionsApi.Config       (defaultConfig, Config(..), Environment(..), setLogger, makePool, lookupSetting)
import MinionsApi.Schema       (runMigrations)

testApp :: IO Application
testApp = do
  env <- lookupSetting "ENV"  Test
  pool <- makePool env
  let cfg    = defaultConfig { getPool = pool, getEnv = env }
      logger = setLogger env
  runMigrations pool
  pure $ logger $ app cfg

withTestApp :: SpecWith Application -> Spec
withTestApp = with testApp
