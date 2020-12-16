module Main where

import qualified Cheops.Db as Db
import Cheops.Email.Content (validateAddress)
import Cheops.Email.Server
import Cheops.Email.Server.Handler.Send.Model (dropEmailScema, initEmailSchema)
import qualified Cheops.K8s as K8s
import Cheops.Logger
import Options.Applicative
import System.Environment.Extended
import System.Exit

newtype Opts = Opts {initDb :: Bool}

optsParser :: Parser Opts
optsParser =
  Opts
    <$> flag
      False
      True
      ( mconcat
          [ long "init-db",
            help "Initialize database structure"
          ]
      )

-- TODO: add command line options for sync/async server
main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (optsParser <**> helper)
        ( fullDesc
            <> progDesc "Cheops Email Delivery Service"
            <> header "cheops-email -- an email delivery service!"
        )

defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig defCapacity Nothing

defaultK8sConfig :: K8s.Config
defaultK8sConfig =
  K8s.Config
    { K8s.configPort = 10030,
      K8s.configReady = ["ready"],
      K8s.configHealth = ["health"],
      K8s.configPreStopHook = ["stop"]
    }

run :: Opts -> IO ()
run Opts {initDb = True} = do
  dbConfig <- decodeEnvironment >>= orDie
  withLogger defaultLoggerConfig $ \ctx -> do
    db <- Db.new dbConfig
    dropEmailScema ctx db
    initEmailSchema ctx db
run _ = do
  dbConfig' <- decodeEnvironment >>= orDie
  serverConfig' <- decodeEnvironment >>= orDie
  timeoutConfig' <- decodeEnvironment >>= orDie
  smtpConfigUnvalidated <- decodeEnvironment >>= orDie

  case validateAddress $ smtpSender smtpConfigUnvalidated of
    Just validatedAddress -> do
      let smtpConfig' = smtpConfigUnvalidated {smtpSender = validatedAddress}
      let config =
            Config
              { loggerConfig = defaultLoggerConfig,
                k8sConfig = defaultK8sConfig,
                serverConfig = serverConfig',
                dbConfig = dbConfig',
                smtpConfig = smtpConfig',
                timeoutConfig = timeoutConfig'
              }
      runEmailService config
    Nothing -> putStrLn "Error: invalid sender email"

orDie :: Either String a -> IO a
orDie (Left e) = die e
orDie (Right x) = pure x
