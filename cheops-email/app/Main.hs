module Main where

import qualified Cheops.Db as Db
import Cheops.Email.Server
import qualified Cheops.K8s as K8s
import Cheops.Logger
import qualified Hasql.Connection.Extended as HC

k8sConfig' :: K8s.Config
k8sConfig' =
  K8s.Config
    { K8s.configPort = 10022,
      K8s.configReady = ["ready"],
      K8s.configHealth = ["health"],
      K8s.configPreStopHook = ["stop"]
    }

loggerConfig' :: LoggerConfig
loggerConfig' = LoggerConfig defCapacity Nothing

serverConfig' :: ServerConfig
serverConfig' = ServerConfig {serverConfigPort = 8000}

dbConfig' :: Db.Config
dbConfig' =
  Db.Config
    { Db.configPostgres =
        Just $
          HC.PostgresConfig
            { HC.postgresConfigHost = HC.Host "localhost",
              HC.postgresConfigPort = HC.Port 5432,
              HC.postgresConfigUser = HC.User "cheops",
              HC.postgresConfigPassword = HC.Password "this_is_sparta",
              HC.postgresConfigDatabase = HC.Database "cheops"
            },
      Db.configPostgresRaw = Nothing,
      Db.configStripes = 1,
      Db.configIdleTime = 10,
      Db.configResourcePerStripe = 10,
      Db.configConnectRetries = Nothing
    }

config' :: Config
config' =
  Config
    { loggerConfig = loggerConfig',
      k8sConfig = k8sConfig',
      serverConfig = serverConfig',
      dbConfig = dbConfig'
    }

main :: IO ()
main = run config'
