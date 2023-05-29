{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
  ( runBot,
  )
where

import qualified Adapter.PostgreSQL.Adapter as PG
import qualified Adapter.PostgreSQL.Common as PG
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Domain.Bot as B
import qualified Domain.Model as M
import Configuration.Dotenv (parseFile)

newtype BotLib = BotLib {unBotLib :: PG.AppState}

instance M.BotDBModel BotLib where
  getUserById pool uId = runReaderT (PG.getUserById uId) (unBotLib pool)
  insertMsg pool uId txt = runReaderT (PG.insertMsg uId txt) (unBotLib pool)
  createUser pool uId uName = runReaderT (PG.createUser uId uName) (unBotLib pool)
  routineAction pool word = runReaderT (PG.routineAction word) (unBotLib pool)

runBot :: FilePath -> IO ()
runBot envFile = do
  cfg <- getCfg <$> parseFile envFile
  case cfg of
    Left err -> error err
    Right (pgCfg, botConfig) -> do
      PG.withAppState pgCfg $ \pool ->
        B.botStartup (M.botToken botConfig) (B.handleBotAction $ BotLib pool)
  where
    getCfg :: [(String, String)] -> Either String (PG.DBConfig, M.BotConfig)
    getCfg env = do
       pgCfg_ <- PG.readDBConfig env
       botConf_ <- M.readBotConfig env
       pure (pgCfg_, botConf_)

