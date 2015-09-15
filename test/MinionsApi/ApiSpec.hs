{-# LANGUAGE OverloadedStrings #-}

module MinionsApi.ApiSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import SpecHelper

spec :: Spec
spec = do

  withTestApp $
    describe "GET /" $ do
      it "responds with 404" $
        get "/" `shouldRespondWith` 404
      it "responds with 'not found'" $
        get "/" `shouldRespondWith` "not found" { matchStatus = 404 }

  withTestApp $
    describe "GET /minions" $ do
      it "responds with 200" $
        get "/minions" `shouldRespondWith` 200
      it "responds with '[]'" $
        get "/minions" `shouldRespondWith` "[]" { matchStatus = 200 }

