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
import qualified Chat as C
import qualified Data.Set as Set

data BotLib = BotLib {unBotLibPGDB :: PG.AppState, unBotLibBotCfg :: M.BotConfig}

instance M.BotDBModel BotLib where
  getUserById pool uId = runReaderT (PG.getUserById uId) (unBotLibPGDB pool)
  insertMsg pool uId txt = runReaderT (PG.insertMsg uId txt) (unBotLibPGDB pool)
  createUser pool uId uName = runReaderT (PG.createUser uId uName) (unBotLibPGDB pool)
  routineAction pool word = runReaderT (PG.routineAction word) (unBotLibPGDB pool)

instance M.BotOpenAIModel BotLib where
  sendRequestToChat botLib queryMsg = C.sendRequestToChat (M.openAiApikey $ unBotLibBotCfg botLib) queryMsg
  isUserAllowed botLib uId = uId `Set.member` M.openAiAllowedUsers (unBotLibBotCfg botLib)

runBot :: FilePath -> IO ()
runBot envFile = do
  cfg <- getCfg <$> parseFile envFile
  case cfg of
    Left err -> error err
    Right (pgCfg, botConfig) -> do
      PG.withAppState pgCfg $ \pool ->
        B.botStartup botConfig (B.handleBotAction BotLib{unBotLibPGDB = pool, unBotLibBotCfg = botConfig})
  where
    getCfg :: [(String, String)] -> Either String (PG.DBConfig, M.BotConfig)
    getCfg env = do
       pgCfg_ <- PG.readDBConfig env
       botConf_ <- M.readBotConfig env
       pure (pgCfg_, botConf_)

