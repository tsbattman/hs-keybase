{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.Keybase.Chat.Api (
    chatApi
  , ChatApi(..)
  , ChatResult(..)
  , ListResult(..)
  , Conversation(..)
  , Channel(..)
  , SendParams(..)
  , SendOptions(..)
  , Message(..)
  , SendResult(..)
  , RateLimit(..)
  ) where

import Data.Aeson
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.Text as T

import Network.Keybase.Proc

chatApi :: FromJSON r => FilePath -> ChatApi r -> IO (Either String (ChatResult r))
chatApi keybase api = execKeybase keybase ["chat", "api"] (encode api) eitherDecode

data ChatApi r where
  List :: ChatApi ListResult
  Send :: SendParams -> ChatApi SendResult

deriving instance Show (ChatApi r)

instance ToJSON (ChatApi r) where
  toJSON List = object ["method" .= T.pack "list"]
  toJSON (Send sp) = object ["method" .= T.pack "send", "params" .= sp]

newtype ChatResult a = ChatResult {
    chatResult :: a
  } deriving (Eq, Show, Read)

instance FromJSON a => FromJSON (ChatResult a) where
  parseJSON = withObject "Result" $ \o -> ChatResult <$> o .: "result"

data ListResult = ListResult {
    listResultConversations :: [Conversation]
  , listResultOffline :: Bool
  } deriving (Eq, Show, Read)

instance FromJSON ListResult where
  parseJSON = withObject "ListResult" $ \o ->
    ListResult <$>
          o .: "conversations"
      <*> o .: "offline"

data Conversation = Conversation {
    conversationId :: T.Text
  , conversationChannel :: Channel
  , conversationUnread :: Bool
  , conversationActiveAt :: UTCTime
  , conversationActiveAtMs :: UTCTime
  , conversationMemberStatus :: T.Text
  } deriving (Eq, Show, Read)

instance FromJSON Conversation where
  parseJSON = withObject "Conversation" $ \o ->
    Conversation <$>
          o .: "id"
      <*> o .: "channel"
      <*> o .: "unread"
      <*> fmap posixSecondsToUTCTime (o .: "active_at")
      <*> fmap posixMillisToUTCTime (o .: "active_at_ms")
      <*> o .: "member_status"

posixMillisToUTCTime :: Int -> UTCTime
posixMillisToUTCTime millis = (fromIntegral ms / 1000) `addUTCTime` posixSecondsToUTCTime (fromIntegral secs)
  where (secs, ms) = millis `divMod` 1000

data Channel = Channel {
    channelName :: T.Text
  , channelPublic :: Bool
  , channelMembersType :: T.Text
  , channelTopicType :: T.Text
  } deriving (Eq, Show, Read)

instance ToJSON Channel where
  toJSON chan = object [
      "name" .= channelName chan
    , "public" .= channelPublic chan
    , "members_type" .= channelMembersType chan
    , "topic_type" .= channelTopicType chan
    ]

instance FromJSON Channel where
  parseJSON = withObject "Channel" $ \o ->
    Channel <$>
          o .: "name"
      <*> o .: "public"
      <*> o .: "members_type"
      <*> o .: "topic_type"

newtype SendParams = SendParams {
    sendOptions :: SendOptions
  } deriving (Eq, Show, Read)

instance ToJSON SendParams where
 toJSON sp = object [
    "options" .= sendOptions sp
  ]

instance FromJSON SendParams where
  parseJSON = withObject "SendParams" $ \o ->
    SendParams <$> o .: "options"

data SendOptions = SendOptions {
    sendChannel :: Channel
  , sendMessage :: Message
  } deriving (Eq, Show, Read)

instance ToJSON SendOptions where
 toJSON sp = object [
      "channel" .= sendChannel sp
    , "message" .= sendMessage sp
    ]

instance FromJSON SendOptions where
  parseJSON = withObject "SendOptions" $ \o ->
    SendOptions <$>
          o .: "channel"
      <*> o .: "message"

newtype Message = Message {
    messageBody :: T.Text
  } deriving (Eq, Show, Read)

instance ToJSON Message where
 toJSON msg = object [
    "body" .= messageBody msg
  ]

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o ->
    Message <$> o .: "body"

data SendResult = SendResult {
    sendResultMessage :: T.Text
  , sendRateLimits :: [RateLimit]
  } deriving (Eq, Show, Read)

instance FromJSON SendResult where
  parseJSON = withObject "SendResult" $ \o ->
    SendResult <$>
          o .: "message"
      <*> o .: "ratelimits"

data RateLimit = RateLimit {
    rateLimitTank :: T.Text
  , rateLimitCapacity :: Int
  , rateLimitReset :: Int
  , rateLimitGas :: Int
  } deriving (Eq, Show, Read)

instance FromJSON RateLimit where
  parseJSON = withObject "RateLimit" $ \o ->
    RateLimit <$>
          o .: "tank"
      <*> o .: "capacity"
      <*> o .: "reset"
      <*> o .: "gas"
