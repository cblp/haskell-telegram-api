{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Web.Telegram.API.Bot.API
  ( -- * Types
    Token(..)
    -- * API
  , TelegramBotAPI
  , api
    -- ** Methods
  , answerCallbackQuery
  , answerInlineQuery
  , editMessageCaption
  , editMessageCaption_
  , editMessageReplyMarkup
  , editMessageReplyMarkup_
  , editMessageText
  , editMessageText_
  , forwardMessage
  , getChatAdministrators
  , getChatMember
  , getChatMembersCount
  , getChat
  , getFile
  , getMe
  , getUpdates
  , getUserProfilePhotos
  , getWebhookInfo
  , kickChatMember
  , leaveChat
  , sendAudio
  , sendChatAction
  , sendContact
  , sendDocument
  , sendGame
  , sendLocation
  , sendMessage
  , sendPhoto
  , sendSticker
  , sendVenue
  , sendVideo
  , sendVoice
  , setWebhookWithCert
  , setWebhook
  , unbanChatMember
  , uploadAudio
  , uploadDocument
  , uploadPhoto
  , uploadSticker
  , uploadVideo
  , uploadVoice
    -- * Running ClientM
  , runClientEx
  ) where

import           Data.Monoid ((<>))
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as Text
import           Network.HTTP.Client (Manager)
import           Servant.API
import           Servant.Client
import           Servant.Client.MultipartFormData
import           Web.Telegram.API.Bot.Responses
import           Web.Telegram.API.Bot.Requests

-- | Telegram Bot's Token
newtype Token = Token Text
  deriving (Show, Eq, Ord, ToHttpApiData, FromHttpApiData)

-- | Telegram Bot API
type TelegramBotAPI =
         "getMe"
         :> Get '[JSON] GetMeResponse
    :<|> "sendMessage"
         :> ReqBody '[JSON] SendMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> "forwardMessage"
         :> ReqBody '[JSON] ForwardMessageRequest
         :> Post '[JSON] MessageResponse
    :<|> "sendPhoto"
         :> MultipartFormDataReqBody (SendPhotoRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> "sendPhoto"
         :> ReqBody '[JSON] (SendPhotoRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> "sendAudio"
         :> MultipartFormDataReqBody (SendAudioRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> "sendAudio"
         :> ReqBody '[JSON] (SendAudioRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> "sendDocument"
         :> MultipartFormDataReqBody (SendDocumentRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> "sendDocument"
         :> ReqBody '[JSON] (SendDocumentRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> "sendSticker"
         :> MultipartFormDataReqBody (SendStickerRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> "sendSticker"
         :> ReqBody '[JSON] (SendStickerRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> "sendVideo"
         :> MultipartFormDataReqBody (SendVideoRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> "sendVideo"
         :> ReqBody '[JSON] (SendVideoRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> "sendVoice"
         :> MultipartFormDataReqBody (SendVoiceRequest FileUpload)
         :> Post '[JSON] MessageResponse
    :<|> "sendVoice"
         :> ReqBody '[JSON] (SendVoiceRequest Text)
         :> Post '[JSON] MessageResponse
    :<|> "sendLocation"
         :> ReqBody '[JSON] SendLocationRequest
         :> Post '[JSON] MessageResponse
    :<|> "sendVenue"
         :> ReqBody '[JSON] SendVenueRequest
         :> Post '[JSON] MessageResponse
    :<|> "sendContact"
         :> ReqBody '[JSON] SendContactRequest
         :> Post '[JSON] MessageResponse
    :<|> "sendChatAction"
         :> ReqBody '[JSON] SendChatActionRequest
         :> Post '[JSON] ChatActionResponse
    :<|> "sendGame"
         :> ReqBody '[JSON] SendGameRequest
         :> Post '[JSON] MessageResponse
    :<|> "getUpdates"
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> QueryParam "timeout" Int
         :> Get '[JSON] UpdatesResponse
    :<|> "getFile"
         :> QueryParam "file_id" Text
         :> Get '[JSON] FileResponse
    :<|> "getUserProfilePhotos"
         :> QueryParam "user_id" Int
         :> QueryParam "offset" Int
         :> QueryParam "limit" Int
         :> Get '[JSON] UserProfilePhotosResponse
    :<|> "setWebhook"
         :> QueryParam "url" Text
         :> Get '[JSON] SetWebhookResponse
    :<|> "setWebhook"
         :> MultipartFormDataReqBody SetWebhookRequest
         :> Post '[JSON] SetWebhookResponse
    :<|> "getWebhookInfo"
         :> Get '[JSON] GetWebhookInfoResponse
    :<|> "answerInlineQuery"
         :> ReqBody '[JSON] AnswerInlineQueryRequest
         :> Post '[JSON] InlineQueryResponse
    :<|> "answerCallbackQuery"
         :> ReqBody '[JSON] AnswerCallbackQueryRequest
         :> Post '[JSON] CallbackQueryResponse
    :<|> "kickChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] KickChatMemberResponse
    :<|> "leaveChat"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] LeaveChatResponse
    :<|> "unbanChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] UnbanChatMemberResponse
    :<|> "getChat"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] GetChatResponse
    :<|> "getChatAdministrators"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] GetChatAdministratorsResponse
    :<|> "getChatMembersCount"
         :> QueryParam "chat_id" Text
         :> Post '[JSON] GetChatMembersCountResponse
    :<|> "getChatMember"
         :> QueryParam "chat_id" Text
         :> QueryParam "user_id" Int
         :> Post '[JSON] GetChatMemberResponse
    :<|> "editMessageText"
         :> ReqBody '[JSON] EditMessageTextRequest
         :> Post '[JSON] MessageResponse
    :<|> "editMessageCaption"
         :> ReqBody '[JSON] EditMessageCaptionRequest
         :> Post '[JSON] MessageResponse
    :<|> "editMessageReplyMarkup"
         :> ReqBody '[JSON] EditMessageReplyMarkupRequest
         :> Post '[JSON] MessageResponse
    :<|> "editMessageText"
         :> ReqBody '[JSON] EditMessageTextRequest
         :> Post '[JSON] (Response Bool)
    :<|> "editMessageCaption"
         :> ReqBody '[JSON] EditMessageCaptionRequest
         :> Post '[JSON] (Response Bool)
    :<|> "editMessageReplyMarkup"
         :> ReqBody '[JSON] EditMessageReplyMarkupRequest
         :> Post '[JSON] (Response Bool)

-- | Proxy for Thelegram Bot API
api :: Proxy TelegramBotAPI
api = Proxy

getMe                     :: ClientM GetMeResponse
sendMessage               :: SendMessageRequest -> ClientM MessageResponse
forwardMessage            :: ForwardMessageRequest -> ClientM MessageResponse
uploadPhoto               :: SendPhotoRequest FileUpload -> ClientM MessageResponse
sendPhoto                 :: SendPhotoRequest Text -> ClientM MessageResponse
uploadAudio               :: SendAudioRequest FileUpload -> ClientM MessageResponse
sendAudio                 :: SendAudioRequest Text -> ClientM MessageResponse
uploadDocument            :: SendDocumentRequest FileUpload -> ClientM MessageResponse
sendDocument              :: SendDocumentRequest Text -> ClientM MessageResponse
uploadSticker             :: SendStickerRequest FileUpload -> ClientM MessageResponse
sendSticker               :: SendStickerRequest Text -> ClientM MessageResponse
uploadVideo               :: SendVideoRequest FileUpload -> ClientM MessageResponse
sendVideo                 :: SendVideoRequest Text -> ClientM MessageResponse
uploadVoice               :: SendVoiceRequest FileUpload -> ClientM MessageResponse
sendVoice                 :: SendVoiceRequest Text -> ClientM MessageResponse
sendLocation              :: SendLocationRequest -> ClientM MessageResponse
sendVenue                 :: SendVenueRequest-> ClientM MessageResponse
sendContact               :: SendContactRequest -> ClientM MessageResponse
sendChatAction            :: SendChatActionRequest -> ClientM ChatActionResponse
sendGame                  :: SendGameRequest -> ClientM MessageResponse
getUpdates                :: Maybe Int -> Maybe Int -> Maybe Int -> ClientM UpdatesResponse
getFile                   :: Maybe Text -> ClientM FileResponse
getUserProfilePhotos      :: Maybe Int -> Maybe Int -> Maybe Int -> ClientM UserProfilePhotosResponse
setWebhook                :: Maybe Text -> ClientM SetWebhookResponse
setWebhookWithCert        :: SetWebhookRequest -> ClientM SetWebhookResponse
getWebhookInfo            :: ClientM GetWebhookInfoResponse
answerInlineQuery         :: AnswerInlineQueryRequest -> ClientM InlineQueryResponse
answerCallbackQuery       :: AnswerCallbackQueryRequest -> ClientM CallbackQueryResponse
kickChatMember            :: Maybe Text -> Maybe Int -> ClientM KickChatMemberResponse
leaveChat                 :: Maybe Text -> ClientM LeaveChatResponse
unbanChatMember           :: Maybe Text -> Maybe Int -> ClientM UnbanChatMemberResponse
getChat                   :: Maybe Text -> ClientM GetChatResponse
getChatAdministrators     :: Maybe Text -> ClientM GetChatAdministratorsResponse
getChatMembersCount       :: Maybe Text -> ClientM GetChatMembersCountResponse
getChatMember             :: Maybe Text -> Maybe Int -> ClientM GetChatMemberResponse
editMessageText           :: EditMessageTextRequest -> ClientM MessageResponse
editMessageCaption        :: EditMessageCaptionRequest -> ClientM MessageResponse
editMessageReplyMarkup    :: EditMessageReplyMarkupRequest -> ClientM MessageResponse
editMessageText_          :: EditMessageTextRequest -> ClientM (Response Bool)
editMessageCaption_       :: EditMessageCaptionRequest -> ClientM (Response Bool)
editMessageReplyMarkup_   :: EditMessageReplyMarkupRequest -> ClientM (Response Bool)
getMe
  :<|> sendMessage
  :<|> forwardMessage
  :<|> uploadPhoto
  :<|> sendPhoto
  :<|> uploadAudio
  :<|> sendAudio
  :<|> uploadDocument
  :<|> sendDocument
  :<|> uploadSticker
  :<|> sendSticker
  :<|> uploadVideo
  :<|> sendVideo
  :<|> uploadVoice
  :<|> sendVoice
  :<|> sendLocation
  :<|> sendVenue
  :<|> sendContact
  :<|> sendChatAction
  :<|> sendGame
  :<|> getUpdates
  :<|> getFile
  :<|> getUserProfilePhotos
  :<|> setWebhook
  :<|> setWebhookWithCert
  :<|> getWebhookInfo
  :<|> answerInlineQuery
  :<|> answerCallbackQuery
  :<|> kickChatMember
  :<|> leaveChat
  :<|> unbanChatMember
  :<|> getChat
  :<|> getChatAdministrators
  :<|> getChatMembersCount
  :<|> getChatMember
  :<|> editMessageText
  :<|> editMessageCaption
  :<|> editMessageReplyMarkup
  :<|> editMessageText_
  :<|> editMessageCaption_
  :<|> editMessageReplyMarkup_ =
      client api

runClientEx :: BaseUrl
            -> Token
            -> Manager
            -> ClientM response
            -> IO (Either ServantError response)
runClientEx baseUrl token manager clientProc =
  let Token tok = token
      url = baseUrl {baseUrlPath = baseUrlPath baseUrl <> "/bot" <> Text.unpack tok}
  in runClientM clientProc (ClientEnv manager url)
