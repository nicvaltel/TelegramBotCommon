{-# LANGUAGE OverloadedStrings #-}

module Chat (sendRequestToChat) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import OpenAI.Client

request :: T.Text -> ChatCompletionRequest
request queryMsg =
  ChatCompletionRequest
    { chcrModel = ModelId "gpt-3.5-turbo",
      chcrMessages =
        [ ChatMessage
            { chmContent = queryMsg,
              chmRole = "user"
            }
        ],
      chcrTemperature = Nothing,
      chcrTopP = Nothing,
      chcrN = Nothing,
      chcrStream = Nothing,
      chcrStop = Nothing,
      chcrMaxTokens = Nothing,
      chcrPresencePenalty = Nothing,
      chcrFrequencyPenalty = Nothing,
      chcrLogitBias = Nothing,
      chcrUser = Nothing
    }

sendRequestToChat :: T.Text -> T.Text -> IO (Either String T.Text)
sendRequestToChat apiKey queryMsg =
  do
    manager <- newManager tlsManagerSettings
    -- apiKey <- T.pack <$> getEnv "OPENAI_KEY"
    -- create a openai client that automatically retries up to 4 times on network errors
    let client = makeOpenAIClient apiKey manager 4
    result <- completeChat client (request queryMsg)
    case result of
      Left failure -> pure $ Left (show failure)
      Right success -> pure $ Right $ T.concat $ map extractContentFromChatChoise $ chrChoices success


extractContentFromChatChoise :: ChatChoice -> T.Text
extractContentFromChatChoise = chmContent . chchMessage

run :: T.Text -> IO ()
run queryMsg = do
  result <- sendRequestToChat (T.pack "") queryMsg
  case result of
    Left err -> print err
    Right answer -> T.putStrLn answer



