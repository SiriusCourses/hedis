module Cheops.Email.Server
  ( ServerConfig (..),
    Config (..),
    SmtpConfig (..),
    runEmailService,
  )
where

import qualified Cheops.Db as Db
import Cheops.Email.Api
import Cheops.Email.Content
import Cheops.Email.Server.Handler.Send
import qualified Cheops.K8s as K8s
import Cheops.Logger
import Cheops.RequestLogger
import Control.Exception hiding (Handler)
import Data.Proxy
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Handler.Warp as Wrap
import Servant
import Servant.Swagger.UI
import Sirius.Response
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
    smtpConfig :: !SmtpConfig
  }

type API =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> EmailApi

server :: LoggerEnv -> SmtpConfig -> Server API
server logEnv smtp =
  swaggerSchemaUIServer emailSwagger
    :<|> emailServer logEnv smtp

emailServer :: LoggerEnv -> SmtpConfig -> Server EmailApi
emailServer logEnv smtp =
  stateHandle
    :<|> sendHandle logEnv smtp
  where
    stateHandle :: Int -> Handler (Response EmailState)
    stateHandle _ = pure $ Ok NotYetSent

emailApp :: LoggerEnv -> SmtpConfig -> Application
emailApp logEnv smtp = serve (Proxy :: Proxy API) (server logEnv smtp)

runEmailService :: Config -> IO ()
runEmailService Config {..} = do
  let serverPort = serverConfigPort serverConfig
  withLogger loggerConfig $ \logEnv -> logSomeException logEnv $ do
    logInfo logEnv $ "Running Email Delivery Service on http://localhost:" <> showLS serverPort

    db <- Db.new dbConfig

    loggerKey <- Vault.newKey
    userKey <- Vault.newKey

    K8s.run k8sConfig (Db.health db logEnv) $
      Wrap.run serverPort $
        loggerMiddleware
          logEnv
          loggerKey
          userKey
          $ emailApp logEnv smtpConfig
  where
    logSomeException ctx f =
      try f >>= \case
        Left se -> logErr ctx $ showLS (se :: SomeException)
        Right () -> pure ()
