module Cheops.Email.Server
  ( ServerConfig (..),
    Config (..),
    SmtpConfig (..),
    runEmailService,
  )
where

import qualified Cheops.Db as Db
import Cheops.Email.Api
import Cheops.Email.Server.Handler.Send
import qualified Cheops.K8s as K8s
import Cheops.Logger
import Cheops.RequestLogger
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Exception hiding (Handler)
import Control.Monad
import Data.Proxy
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Handler.Warp as Wrap
import Servant
import Servant.Swagger.UI
import System.Environment.Extended

newtype ServerConfig = ServerConfig {serverConfigPort :: Int}
  deriving (Show)

instance FromEnv ServerConfig where
  fromEnv env = ServerConfig <$> (env .-> "EMAIL_SERVER_PORT")

data Config = Config
  { loggerConfig :: !LoggerConfig,
    k8sConfig :: !K8s.Config,
    serverConfig :: !ServerConfig,
    dbConfig :: !Db.Config,
    smtpConfig :: !SmtpConfig,
    timeoutConfig :: !TimeoutConfig
  }

type API =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> EmailApi

data EmailAppInfo = EmailAppInfo
  { emailAppLogger :: LoggerEnv,
    emailAppDbHandle :: Db.Handle,
    emailAppNotify :: STM ()
  }

server :: EmailAppInfo -> Server API
server appInfo =
  swaggerSchemaUIServer emailSwagger
    :<|> emailServer appInfo

emailServer :: EmailAppInfo -> Server EmailApi
emailServer (EmailAppInfo logEnv db notify) =
  stateHandle logEnv db
    :<|> sendHandleAsync logEnv db notify

emailApp :: EmailAppInfo -> Application
emailApp appInfo = serve (Proxy :: Proxy API) (server appInfo)

recieverService :: Config -> LoggerEnv -> Db.Handle -> STM () -> IO ()
recieverService Config {..} logEnv db notify = do
  let serverPort = serverConfigPort serverConfig
  logInfo logEnv $ "Running Email Delivery Service on http://localhost:" <> showLS serverPort

  loggerKey <- Vault.newKey
  userKey <- Vault.newKey

  K8s.run k8sConfig (Db.health db logEnv) $
    Wrap.run serverPort $
      loggerMiddleware
        logEnv
        loggerKey
        userKey
        $ emailApp (EmailAppInfo logEnv db notify)

mkCondVariable :: IO (STM (), STM ())
mkCondVariable = do
  ref <- newTVarIO False
  pure
    ( writeTVar ref True,
      readTVar ref >>= guard >> writeTVar ref False
    )

runEmailService :: Config -> IO ()
runEmailService config@Config {..} = do
  withLogger loggerConfig $ \logEnv -> logSomeException logEnv $ do
    db <- Db.new dbConfig

    (notify, waitCond) <- mkCondVariable

    let recieverCtx = addNamespace "reciever-worker" logEnv
    let workerCtx = addNamespace "send-worker" logEnv

    race_
      (recieverService config recieverCtx db notify)
      (sendWorker workerCtx db smtpConfig timeoutConfig waitCond)
  where
    logSomeException ctx f =
      try f >>= \case
        Left se -> logErr ctx $ showLS (se :: SomeException)
        Right () -> pure ()
