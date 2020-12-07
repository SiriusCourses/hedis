module Cheops.Email.Api where

import Cheops.Email.Content
import Control.Lens
import Data.Swagger
import Servant
import Servant.Swagger

type EmailApi =
  "state" :> Capture "stateid" Int :> Get '[JSON] EmailState
    :<|> "send" :> ReqBody '[JSON] EmailContent :> Post '[JSON] Int

emailSwagger :: Swagger
emailSwagger =
  toSwagger (Proxy :: Proxy EmailApi)
    & info . title .~ "Cheops Email Delivary Service API"
    & info . version .~ "1.0"
    & info . description ?~ "This is an interactive documentation for Cheops Email Delivery Service"
