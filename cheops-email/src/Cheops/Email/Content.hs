{-# LANGUAGE StandaloneDeriving #-}

module Cheops.Email.Content where

import Cheops.Error
import Control.Lens
import Data.Aeson
import Data.Int
import Data.Swagger
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Hasql.Decoders as HD
import Network.Mail.Mime
import Servant.API.Generic
import Text.Email.Validate

data EmailError
  = AuthError
  | SendError
  | InvalidEmailError
  | InternalError
  deriving (Generic, Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance AsError EmailError where
  asError AuthError = Cheops.Error.Error "Ошибка аутентификации" Nothing
  asError SendError = Cheops.Error.Error "Ошибка при попытке отправить письмо" Nothing
  asError InvalidEmailError = Cheops.Error.Error "Некорректный адрес электронной почты" Nothing
  asError InternalError = Cheops.Error.Error "Внутренняя ошибка сервиса" Nothing

data EmailState
  = EmailStateSent
  | EmailStatePending
  | EmailStateProcessing
  | EmailStateError
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

_EmailStateText :: Prism' T.Text EmailState
_EmailStateText = prism toTextual $ \case
  "sent" -> Right EmailStateSent
  "pending" -> Right EmailStatePending
  "processing" -> Right EmailStateProcessing
  "error" -> Right EmailStateError
  e -> Left e
  where
    toTextual EmailStateSent = "sent"
    toTextual EmailStatePending = "pending"
    toTextual EmailStateProcessing = "processing"
    toTextual EmailStateError = "error"

instance HD.AsValue EmailState where value = HD.enumP _EmailStateText

type EmailTaskId = Int64

data Body = Body
  { bodyPlain :: T.Text,
    bodyHtml :: T.Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data EmailContent = EmailContent
  { emailTo :: Address,
    emailCc :: [Address],
    emailBcc :: [Address],
    emailSubject :: T.Text,
    emailBody :: Body
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

deriving instance Generic Address

instance ToJSON Address

instance FromJSON Address

instance ToSchema Address

-- | Validate email and canonicalize email addresses.
validateEmail :: EmailContent -> Maybe EmailContent
validateEmail email =
  let mEmailToValidated = validateAddress $ emailTo email
      mEmailCcValidated = mapM validateAddress $ emailCc email
      mEmailBccValidated = mapM validateAddress $ emailBcc email
   in updateAddrs email <$> mEmailToValidated <*> mEmailCcValidated <*> mEmailBccValidated
  where
    updateAddrs email to cc bcc = email {emailTo = to, emailCc = cc, emailBcc = bcc}

-- | Validate and canonicalize email address and
--
-- Note:
-- Ignore error message, because it's too confusing:
--   > validate "not good "
--   Left "at sign > @: Failed reading: satisfyWith"
validateAddress :: Address -> Maybe Address
validateAddress addr@(Address _ email) =
  case validate $ encodeUtf8 email of
    Right validEmail -> Just $ addr {addressEmail = decodeUtf8 $ toByteString validEmail}
    Left _ -> Nothing
