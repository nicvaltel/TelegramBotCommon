{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Bot
  ( botStartup,
    ChatModel (..),
    Action (..),
    handleBotAction,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe
import Data.Text (Text, pack)
import Debug.Trace (traceShow)
import qualified Domain.Model as M
import Telegram.Bot.API as Telegram
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser


data ChatState
  = InitSate
  deriving (Show, Eq)

newtype ChatModel
  = ChatModel ChatState
  deriving (Show, Eq)

data Action
  = NoAction
  | RecordMsg Int (Maybe Text) Int Text
  deriving (Show, Read)

botStartup :: (MonadIO m) => M.BotConfig -> (Action -> ChatModel -> Eff Action ChatModel) -> m ()
botStartup botConf handleAction = do
  let token = Token . pack $ M.botToken botConf
  env <- liftIO $ defaultTelegramClientEnv token
  liftIO $ startBot_ (conversationBot updateChatId (incexpBotApp handleAction)) env

emptyChatModel :: ChatModel
emptyChatModel = ChatModel InitSate

incexpBotApp :: (Action -> ChatModel -> Eff Action ChatModel) -> BotApp ChatModel Action
incexpBotApp handleAction = BotApp {botInitialModel = emptyChatModel, botAction = flip handleUpdate, botHandler = handleAction, botJobs = []}

handleUpdate :: ChatModel -> Update -> Maybe Action
handleUpdate _ update = do
  msg <- updateMessage update
  usr <- messageFrom msg
  let Telegram.UserId usrId = Telegram.userId usr
  let Telegram.MessageId msgId = Telegram.messageMessageId msg
  let usrIdInt = fromIntegral usrId :: Int
  let msgIdInt = fromIntegral msgId :: Int
  let usrName = Telegram.userUsername usr
  let parser = RecordMsg usrIdInt usrName msgIdInt <$> plainText
  parseUpdate parser update

handleBotAction :: M.BotDBModel a => a -> Action -> ChatModel -> Eff Action ChatModel
handleBotAction botDBModel action model = traceShow action $
  case action of
    NoAction -> pure model
    RecordMsg usrId mayUsrname _ userMsg -> do
      let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
      model <# do
        maybeUser :: Maybe M.User <- liftIO $ M.getUserById botDBModel usrId
        when (isNothing maybeUser) $ liftIO $ M.createUser botDBModel usrId usrname >> pure ()
        _ <- liftIO $ M.insertMsg botDBModel usrId userMsg
        case maybeUser of
          Just _ -> actionRoutine botDBModel userMsg
          Nothing -> actionNewUser
        pure NoAction

replyString :: Text -> BotM ()
replyString = reply . toReplyMessage

-- Implement Bot logic here
actionRoutine :: M.BotDBModel a => a -> Text -> BotM ()
actionRoutine botDBModel userMsg = do
  eitherResult <- liftIO $ M.routineAction botDBModel userMsg
  case eitherResult of
    Right res -> replyString res
    Left err -> replyString . pack $ show err

-- Implement Bot greetings here
actionNewUser :: BotM ()
actionNewUser = do
  replyString "Я бот. Моё почтение."
  replyString "I'm a bot. Greetings"


msgHello, msgTryToRepeat, msgWait :: Text
msgHello = "Я бот-ретранслятор запросов к OpenAI Chat. Напишите вош запрос (т.е. всё что хотите)"
msgTryToRepeat = "Не удалось получить ответ. Попробуйте повторить запрос."
msgWait = "Секундочку..."

handleOpenAIChat :: (M.BotDBModel a, M.BotOpenAIModel a) => a -> Action -> ChatModel -> Eff Action ChatModel
handleOpenAIChat botLib action model = traceShow action $
  case action of
    NoAction -> pure model
    RecordMsg usrId mayUsrname _ word ->
      if M.isUserAllowed botLib usrId
        then do
          let usrname = fromMaybe (pack $ "user_" <> show usrId) mayUsrname
          model <# do
            maybeUser :: Maybe M.User <- liftIO $ M.getUserById botLib usrId
            when (isNothing maybeUser) $ liftIO $ M.createUser botLib usrId usrname >> pure ()
            case maybeUser of
              Just _ -> do
                replyString msgWait
                chatAns <- liftIO $  M.sendRequestToChat botLib word
                case chatAns of
                  Right answer -> replyString answer
                  Left _ -> replyString msgTryToRepeat
                msgSaveResult <- liftIO $ M.insertMsg botLib usrId word
                case msgSaveResult of
                  Right _ -> pure ()
                  Left err -> replyString . pack $ show err
              Nothing -> replyString msgHello
            pure NoAction
        else pure model
