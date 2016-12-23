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
  , answerCallbackQuery_
  , answerInlineQuery_
  , editMessageCaption_
  , editMessageCaption__
  , editMessageReplyMarkup_
  , editMessageReplyMarkup__
  , editMessageText_
  , editMessageText__
  , forwardMessage_
  , getChatAdministrators_
  , getChatMember_
  , getChatMembersCount_
  , getChat_
  , getFile_
  , getMe_
  , getUpdates_
  , getUserProfilePhotos_
  , kickChatMember_
  , leaveChat_
  , sendAudio_
  , sendChatAction_
  , sendContact_
  , sendDocument_
  , sendGame_
  , sendLocation_
  , sendMessage_
  , sendPhoto_
  , sendSticker_
  , sendVenue_
  , sendVideo_
  , sendVoice_
  , setWebhookWithCert_
  , setWebhook_
  , getWebhookInfo_
  , unbanChatMember_
  , uploadAudio_
  , uploadDocument_
  , uploadPhoto_
  , uploadSticker_
  , uploadVideo_
  , uploadVoice_
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

getMe_                     :: ClientM GetMeResponse
sendMessage_               :: SendMessageRequest -> ClientM MessageResponse
forwardMessage_            :: ForwardMessageRequest -> ClientM MessageResponse
uploadPhoto_               :: SendPhotoRequest FileUpload -> ClientM MessageResponse
sendPhoto_                 :: SendPhotoRequest Text -> ClientM MessageResponse
uploadAudio_               :: SendAudioRequest FileUpload -> ClientM MessageResponse
sendAudio_                 :: SendAudioRequest Text -> ClientM MessageResponse
uploadDocument_            :: SendDocumentRequest FileUpload -> ClientM MessageResponse
sendDocument_              :: SendDocumentRequest Text -> ClientM MessageResponse
uploadSticker_             :: SendStickerRequest FileUpload -> ClientM MessageResponse
sendSticker_               :: SendStickerRequest Text -> ClientM MessageResponse
uploadVideo_               :: SendVideoRequest FileUpload -> ClientM MessageResponse
sendVideo_                 :: SendVideoRequest Text -> ClientM MessageResponse
uploadVoice_               :: SendVoiceRequest FileUpload -> ClientM MessageResponse
sendVoice_                 :: SendVoiceRequest Text -> ClientM MessageResponse
sendLocation_              :: SendLocationRequest -> ClientM MessageResponse
sendVenue_                 :: SendVenueRequest-> ClientM MessageResponse
sendContact_               :: SendContactRequest -> ClientM MessageResponse
sendChatAction_            :: SendChatActionRequest -> ClientM ChatActionResponse
sendGame_                  :: SendGameRequest -> ClientM MessageResponse
getUpdates_                :: Maybe Int -> Maybe Int -> Maybe Int -> ClientM UpdatesResponse
getFile_                   :: Maybe Text -> ClientM FileResponse
getUserProfilePhotos_      :: Maybe Int -> Maybe Int -> Maybe Int -> ClientM UserProfilePhotosResponse
setWebhook_                :: Maybe Text -> ClientM SetWebhookResponse
setWebhookWithCert_        :: SetWebhookRequest -> ClientM SetWebhookResponse
getWebhookInfo_            :: ClientM GetWebhookInfoResponse
answerInlineQuery_         :: AnswerInlineQueryRequest -> ClientM InlineQueryResponse
answerCallbackQuery_       :: AnswerCallbackQueryRequest -> ClientM CallbackQueryResponse
kickChatMember_            :: Maybe Text -> Maybe Int -> ClientM KickChatMemberResponse
leaveChat_                 :: Maybe Text -> ClientM LeaveChatResponse
unbanChatMember_           :: Maybe Text -> Maybe Int -> ClientM UnbanChatMemberResponse
getChat_                   :: Maybe Text -> ClientM GetChatResponse
getChatAdministrators_     :: Maybe Text -> ClientM GetChatAdministratorsResponse
getChatMembersCount_       :: Maybe Text -> ClientM GetChatMembersCountResponse
getChatMember_             :: Maybe Text -> Maybe Int -> ClientM GetChatMemberResponse
editMessageText_           :: EditMessageTextRequest -> ClientM MessageResponse
editMessageCaption_        :: EditMessageCaptionRequest -> ClientM MessageResponse
editMessageReplyMarkup_    :: EditMessageReplyMarkupRequest -> ClientM MessageResponse
editMessageText__          :: EditMessageTextRequest -> ClientM (Response Bool)
editMessageCaption__       :: EditMessageCaptionRequest -> ClientM (Response Bool)
editMessageReplyMarkup__   :: EditMessageReplyMarkupRequest -> ClientM (Response Bool)
getMe_
  :<|> sendMessage_
  :<|> forwardMessage_
  :<|> uploadPhoto_
  :<|> sendPhoto_
  :<|> uploadAudio_
  :<|> sendAudio_
  :<|> uploadDocument_
  :<|> sendDocument_
  :<|> uploadSticker_
  :<|> sendSticker_
  :<|> uploadVideo_
  :<|> sendVideo_
  :<|> uploadVoice_
  :<|> sendVoice_
  :<|> sendLocation_
  :<|> sendVenue_
  :<|> sendContact_
  :<|> sendChatAction_
  :<|> sendGame_
  :<|> getUpdates_
  :<|> getFile_
  :<|> getUserProfilePhotos_
  :<|> setWebhook_
  :<|> setWebhookWithCert_
  :<|> getWebhookInfo_
  :<|> answerInlineQuery_
  :<|> answerCallbackQuery_
  :<|> kickChatMember_
  :<|> leaveChat_
  :<|> unbanChatMember_
  :<|> getChat_
  :<|> getChatAdministrators_
  :<|> getChatMembersCount_
  :<|> getChatMember_
  :<|> editMessageText_
  :<|> editMessageCaption_
  :<|> editMessageReplyMarkup_
  :<|> editMessageText__
  :<|> editMessageCaption__
  :<|> editMessageReplyMarkup__ =
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
