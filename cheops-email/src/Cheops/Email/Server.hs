module Cheops.Email.Server where

import qualified Cheops.Db as Db
import Cheops.Email.Api
import Cheops.Email.Content
import qualified Cheops.K8s as K8s
import Cheops.Logger
import Cheops.RequestLogger
import Control.Exception hiding (Handler)
import Data.Proxy
import qualified Data.Vault.Lazy as Vault
import Network.Wai.Handler.Warp as Wrap
import Servant
import Servant.Swagger.UI

newtype ServerConfig = ServerConfig {serverConfigPort :: Int}

data Config = Config
  { loggerConfig :: !LoggerConfig,
    k8sConfig :: !K8s.Config,
    serverConfig :: !ServerConfig,
    dbConfig :: !Db.Config
  }

type API =
  SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> EmailApi

server :: Server API
server =
  swaggerSchemaUIServer emailSwagger
    :<|> emailServer

emailServer :: Server EmailApi
emailServer =
  stateHandle
    :<|> sendHandle
  where
    stateHandle _ = pure NotYetSent
    sendHandle _ = pure 0

emailApp :: Application
emailApp = serve (Proxy :: Proxy API) server

run :: Config -> IO ()
run Config {..} = do
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
          emailApp
  where
    logSomeException ctx f =
      try f >>= \case
        Left se -> logErr ctx $ showLS (se :: SomeException)
        Right () -> pure ()
