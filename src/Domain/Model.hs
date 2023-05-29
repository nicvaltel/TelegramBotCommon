{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Model
  ( BotDBModel (..),
    BotConfig(..),
    UserId,
    Username,
    MessageId,
    User (..),
    Message (..),
    MessageError (..),
    ActionError (..),
    readBotConfig
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Either.Combinators (maybeToRight)

newtype BotConfig = BotConfig {
  botToken :: String
}

type UserId = Int

type Username = Text

type MessageId = Int

data User = User
  { userId :: UserId,
    username :: Username,
    created :: UTCTime
  }
  deriving (Show, Eq)

data Message = Message
  { messageId :: MessageId,
    uId :: Int,
    text :: Text,
    sent :: UTCTime
  }
  deriving (Show, Eq)

newtype MessageError = UserDoesNotExist UserId
  deriving (Show, Eq)

data ActionError = ActionError deriving (Show)

class BotDBModel a where
  getUserById :: (MonadIO m, MonadThrow m) => a -> UserId -> m (Maybe User)
  insertMsg :: (MonadIO m, MonadThrow m) => a -> UserId -> Text -> m (Either MessageError Message)
  createUser :: (MonadIO m, MonadThrow m) => a -> UserId -> Username -> m User
  routineAction :: (MonadIO m, MonadThrow m) => a -> Text -> m (Either ActionError Text)


readBotConfig :: [(String, String)] -> Either String BotConfig
readBotConfig env = do
  botToken <- maybeToRight "No TOKEN defined" (lookup "BOT_TOKEN" env)
  pure BotConfig {botToken}
