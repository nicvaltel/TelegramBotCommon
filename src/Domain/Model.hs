{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Domain.Model
  ( BotDBModel (..),
    BotOpenAIModel (..),
    BotConfig (..),
    UserId,
    Username,
    MessageId,
    User (..),
    Message (..),
    MessageError (..),
    ActionError (..),
    readBotConfig,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Data.Either.Combinators (maybeToRight)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

data BotConfig = BotConfig
  { botToken :: String,
    openAiApikey :: T.Text,
    openAiAllowedUsers :: Set Int
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

class BotOpenAIModel a where
  sendRequestToChat :: a -> T.Text -> IO (Either String T.Text)
  isUserAllowed :: a -> UserId -> Bool

readBotConfig :: [(String, String)] -> Either String BotConfig
readBotConfig env = do
  botToken <- maybeToRight "No TOKEN defined" (lookup "BOT_TOKEN" env)
  openAiApikey <- maybeToRight "No API_KEY defined" (T.pack <$> lookup "API_KEY" env)
  openAiAllowedUsers <- maybeToRight "No ALLOWED_USERS defined" (read <$> lookup "ALLOWED_USERS" env)
  pure BotConfig {botToken, openAiApikey, openAiAllowedUsers}
