{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
module Cheops.Email.Content where

import Data.Aeson
import Data.Swagger
import qualified Data.Text as T
import Network.Mail.Mime
import Servant.API.Generic

data EmailState = Sent | NotYetSent | Error {- TODO: add ErrorType -}
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type Body = T.Text

data EmailContent = EmailContent
  { emailTo :: Address,
    emailCc :: [Address],
    emailBcc :: [Address],
    emailSubject :: T.Text,
    emailBody :: Body
  }
  deriving (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

deriving instance Generic Address

instance ToJSON Address

instance FromJSON Address

instance ToSchema Address
