{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cheops.Email.Server.Handler.Send
  ( SmtpConfig (..),
    TimeoutConfig (..),
    sendHandleSync,
    sendHandleAsync,
    sendWorker,
    stateHandle,
  )
where

import qualified Cheops.Db as Db
import Cheops.Email.Content
import qualified Cheops.Email.Server.Handler.Send.Model as Model
import Cheops.Logger
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (guard, join, msum)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as Json
import qualified Data.ByteString.Lazy as B
import Data.Function (fix)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Network.HaskellNet.Auth (AuthType (LOGIN))
import qualified Network.HaskellNet.SMTP.SSL as SMTP
import Network.Mail.Mime
import Servant (Handler)
import Servant.Server (err404)
import Sirius.Response
import System.Environment.Extended
import TextShow

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

data TimeoutConfig = TimeoutConfig
  { retryTimeout :: Int,
    retryTimeoutOnError :: Int
  }

instance FromEnv TimeoutConfig where
  fromEnv env =
    TimeoutConfig
      <$> (env .-> "EMAIL_RETRY_TIMEOUT")
      <*> (env .-> "EMAIL_RETRY_TIMEOUT_ON_ERROR")

data SendException = InvalidEmailException | AuthException

instance Show SendException where
  show InvalidEmailException = "invalid email"
  show AuthException = "authentication error"

instance Exception SendException

stateHandle :: LoggerEnv -> Db.Handle -> EmailTaskId -> Servant.Handler (Response EmailState)
stateHandle logEnv db taskId = liftIO $ do
  mState <- Db.transaction db logEnv $ \txn -> Db.statement txn Model.getTaskStatus taskId
  case mState of
    Nothing -> throwIO err404
    Just state -> pure $ Ok state

sendWorker :: LoggerEnv -> Db.Handle -> SmtpConfig -> TimeoutConfig -> STM () -> IO ()
sendWorker ctx db smtp TimeoutConfig {..} waitCond = retrying $
  fix $ \nextWait -> do
    timeoutPassed <- registerDelay retryTimeout
    wasNotified <-
      atomically $
        msum
          [ readTVar timeoutPassed >>= guard >> pure False,
            waitCond >> pure True
          ]

    logInfo ctx $
      "Worker awakens because "
        <> (if wasNotified then "it was notified" else "timeout was reached")

    fix $ \next -> do
      lockTask ctx db >>= \case
        Model.NoTask -> nextWait
        Model.EncodeError -> nextWait
        Model.HasTask taskId content -> do
          let ctx' = addContext (sl "task_id" taskId) ctx
          processTask ctx' db smtp taskId content
            `catch` abortTask ctx' db taskId
          next
  where
    retrying f = do
      try f >>= \case
        Left se | Just (_e :: AsyncException) <- fromException se -> pure ()
        Left se | Just (_e :: AsyncCancelled) <- fromException se -> pure ()
        Left se -> do
          logErr ctx $ "Retrying because of exception: " <> showLS se
          threadDelay retryTimeoutOnError
          retrying f
        Right x -> pure x

    lockTask ctx db =
      Db.transaction db ctx $ \txn ->
        Db.statement txn Model.lockPendingTask ()

    processTask ctx db smtp taskId content = do
      sendWithAuth smtp content
      finishTask ctx db taskId

    finishTask ctx db taskId = do
      logInfo ctx "Finishing processing task"
      Db.transaction db ctx $ \txn ->
        Db.statement txn Model.finishTask taskId

    abortTask ctx db taskId exception = do
      logErr ctx $ "Aborting processing task: " <> showLS (exception :: SomeException)
      Db.transaction db ctx $ \txn ->
        Db.statement txn Model.abortTaskWithReason (taskId, showt exception)

sendHandleAsync :: LoggerEnv -> Db.Handle -> STM () -> EmailContent -> Servant.Handler (Response EmailTaskId)
sendHandleAsync ctx db notify email = liftIO $ do
  logInfo ctx $ "Register email: " <> showLS email
  try (sendAsync ctx db email) >>= \case
    Left InvalidEmailException -> do
      logErr ctx "Can't register an email: invalid email"
      pure $ Errors [asError InvalidEmailError]
    Left reason -> do
      logErr ctx $ "Can't register an email: " <> showLS (toException reason)
      pure $ Errors [asError InternalError]
    Right taskId -> do
      logInfo ctx "Successfully registered email"
      atomically notify
      pure $ Ok taskId

sendAsync :: LoggerEnv -> Db.Handle -> EmailContent -> IO EmailTaskId
sendAsync ctx db email
  | Just emailValidated <- validateEmail email =
    register ctx emailValidated
  | otherwise =
    throwIO InvalidEmailException
  where
    register :: LoggerEnv -> EmailContent -> IO EmailTaskId
    register ctx content = Db.transaction db ctx $ \txn -> do
      Db.statement txn Model.registerTask content

sendHandleSync :: LoggerEnv -> SmtpConfig -> EmailContent -> Servant.Handler (Response EmailTaskId)
sendHandleSync ctx smtp email = liftIO $ do
  logInfo ctx $ "Sending email: " <> showLS email
  case validateEmail email of
    Just emailValidated -> do
      try (sendWithAuth smtp emailValidated) >>= \case
        Left AuthException -> do
          logErr ctx "Can't send an email: authentication error"
          pure $ Errors [asError AuthError]
        Left reason -> do
          logErr ctx $ "Can't send an email:" <> showLS (toException reason)
          pure $ Errors [asError SendError]
        Right _ -> pure $ Ok 0
    Nothing -> do
      logErr ctx "Can't send an email: invalid email address"
      pure $ Errors [asError InvalidEmailError]

sendWithAuth :: SmtpConfig -> EmailContent -> IO ()
sendWithAuth SmtpConfig {..} content = SMTP.doSMTPSTARTTLS smtpServer $ \conn -> do
  let senderEmail = T.unpack $ addressEmail smtpSender
  authSucceed <- SMTP.authenticate LOGIN senderEmail smtpPassword conn
  if authSucceed
    then sendEmail smtpSender content conn
    else throwIO AuthException

sendEmail :: Address -> EmailContent -> SMTP.SMTPConnection -> IO ()
sendEmail sender email@EmailContent {..} conn = do
  renderedMail <- renderMail' $ toMail sender email
  SMTP.sendMail from to (B.toStrict renderedMail) conn
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
