{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cheops.Email.Server.Handler.Send
  ( SmtpConfig (..),
    sendHandle,
  )
where

import Cheops.Email.Content
import Cheops.Logger
import Control.Exception
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HaskellNet.Auth (AuthType (LOGIN))
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Network.Mail.Mime
import Servant (Handler)
import Sirius.Response
import System.Environment.Extended

data SmtpConfig = SmtpConfig
  { -- | Address of STMP server. For example, stmp.yandex.ru.
    smtpServer :: String,
    smtpSender :: Address,
    smtpPassword :: String
  }

instance ParseEnv String where parseEnv = Right

instance FromEnv SmtpConfig where
  fromEnv env =
    SmtpConfig
      <$> (env .-> "SMTP_SERVER")
      <*> (Address <$> (env ?-> "SMTP_USER_NAME") <*> (env .-> "SMTP_USER_EMAIL"))
      <*> (env .-> "SMTP_USER_PASSWORD")

sendHandle :: LoggerEnv -> SmtpConfig -> EmailContent -> Servant.Handler (Response EmailTaskId)
sendHandle logEnv smtp email = liftIO $ do
  logInfo logEnv $ "Sending email: " <> showLS email
  case validateEmail email of
    Just emailValidated -> do
      result <- sendWithAuth logEnv smtp emailValidated
      -- Note: return task id when async is implemented
      case result of
        Nothing -> pure $ Ok 0
        Just err -> pure $ Errors [asError err]
    Nothing -> do
      logErr logEnv $ "Can't send an email: invalid email address"
      pure $ Errors [asError InvalidEmailError]

sendWithAuth :: LoggerEnv -> SmtpConfig -> EmailContent -> IO (Maybe EmailError)
sendWithAuth logEnv SmtpConfig {..} content = SMTP.doSMTPSTARTTLS smtpServer $ \conn -> do
  let senderEmail = T.unpack $ addressEmail smtpSender
  authSucceed <- SMTP.authenticate LOGIN senderEmail smtpPassword conn
  if authSucceed
    then sendEmail logEnv smtpSender content conn
    else do
      logErr logEnv "Can't send an email: authentication error!"
      pure $ Just AuthError

sendEmail :: LoggerEnv -> Address -> EmailContent -> SMTP.SMTPConnection -> IO (Maybe EmailError)
sendEmail logEnv sender email@EmailContent {..} conn = do
  renderedMail <- renderMail' $ toMail sender email
  try (SMTP.sendMail from to (B.toStrict renderedMail) conn) >>= \case
    Left (e :: SomeException) -> do
      logErr logEnv $ showLS e
      pure $ Just SendError
    Right _ -> pure Nothing
  where
    from = T.unpack $ addressEmail sender
    -- Need to explicitly pass all target emails.
    to = T.unpack . addressEmail <$> (emailTo : (emailCc ++ emailBcc))

toMail :: Address -> EmailContent -> Mail
toMail from EmailContent {..} =
  Mail
    { mailFrom = from,
      mailTo = [emailTo],
      mailCc = emailCc,
      mailBcc = emailBcc,
      mailHeaders = [("Subject", emailSubject)],
      mailParts = toMailParts emailBody
    }
  where
    -- Network.Mail.Mime represents an email body as a list of email parts which
    -- have their own content-type and other metadescription. For our simple
    -- (for now) case, the structure is the following:
    -- [[plaintext version, html version]].
    -- Note that the most preferred version (html, in our case) must be placed
    -- last.
    toMailParts Body {..} =
      [ [ plainPart $ TL.fromStrict bodyPlain,
          htmlPart $ TL.fromStrict bodyHtml
        ]
      ]
