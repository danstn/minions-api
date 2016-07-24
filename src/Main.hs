{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp  (run)

import MinionsApi.Service      (app)
import MinionsApi.Middlewares  (allowCsrf, corsified)
import MinionsApi.Config       (defaultConfig, Config(..), Environment(..), setLogger, makePool, lookupSetting)
import MinionsApi.Schema       (runMigrations)

main :: IO ()
main = do
  env  <- lookupSetting "ENV"  Development
  port <- lookupSetting "PORT" 8081
  pool <- makePool env
  printStatus env port
  let cfg    = defaultConfig { getPool = pool, getEnv = env }
      logger = setLogger env
  runMigrations pool
  run port $ logger $ allowCsrf $ corsified $ app cfg

printStatus :: Environment -> Int -> IO ()
printStatus env port = do
  putStrLn $ "Environment: " ++ show env
  putStrLn $ "Bananas are served at: http://localhost:" ++ show port
