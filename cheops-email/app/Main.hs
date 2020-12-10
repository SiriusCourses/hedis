module Main where

import Cheops.Email.Server
import qualified Cheops.K8s as K8s
import Cheops.Logger
import System.Environment.Extended
import System.Exit
import Cheops.Email.Content (validateAddress)


main :: IO ()
main = do
  dbConfig <- decodeEnvironment >>= orDie
  serverConfig <- decodeEnvironment >>= orDie
  smtpConfigUnvalidated <- decodeEnvironment >>= orDie

  case validateAddress $ smtpSender smtpConfigUnvalidated of
    Just validatedAddress  -> do
      let smtpConfig = smtpConfigUnvalidated { smtpSender = validatedAddress }
      let config = Config {..}
      runEmailService config
    Nothing -> putStrLn "Error: invalid sender email"
  where
    loggerConfig = LoggerConfig defCapacity Nothing
    k8sConfig =
      K8s.Config
        { K8s.configPort = 10030,
          K8s.configReady = ["ready"],
          K8s.configHealth = ["health"],
          K8s.configPreStopHook = ["stop"]
        }

orDie :: Either String a -> IO a
orDie (Left e) = die e
orDie (Right x) = pure x
