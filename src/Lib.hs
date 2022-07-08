{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import Control.Lens (to, view, (&), (.~), (?~), (^?))
import Data.Aeson (FromJSON, ToJSON, Value, decode, decodeFileStrict')
import Data.Aeson.Lens (AsValue (_String), key)
import Data.ByteString.Lazy (ByteString)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Map.Strict (Map)
import Data.Semigroup (Sum (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import Network.HTTP.Client.OpenSSL
  ( opensslManagerSettings,
    withOpenSSL,
  )
import Network.Wreq
  ( FormParam (..),
    Options,
    Response,
    auth,
    defaults,
    getWith,
    manager,
    oauth2Bearer,
    post,
    postWith,
    responseBody,
  )
import OpenSSL.Session (context)

type Usage = Double

type Account = Text

type Record = Map Text Value

type Token = Text

oauthApi :: String
oauthApi = "services/oauth2/token"

getToken :: Options -> IO Token
getToken opts = do
  Just creds <- decodeFileStrict' "creds.json"
  response <-
    fmap (view responseBody) $
      withOpenSSL $
        postWith opts oauthApi $ encodeCreds creds
  case response ^? key "access_token" . _String of
    Nothing -> error "no token"
    Just txt -> pure txt

getAny :: FromJSON a => Options -> String -> IO (Maybe a)
getAny opts q =
  decode . view responseBody
    <$> do withOpenSSL $ getWith opts q

querySalesforce :: String -> IO (Maybe Value) -- IO (Maybe [Record]) --
querySalesforce query = do
  let opts = defaults & manager .~ Left (opensslManagerSettings context)
  token <- getToken opts
  let opts' = opts & auth ?~ oauth2Bearer (Text.encodeUtf8 token)
  getAny opts' "asdnsadnkjenk3"

type Result = MonoidalMap Account (Sum Usage)

computeRanking :: [Record] -> Result
computeRanking xs = MonoidalMap.fromList undefined -- fmap (fmap Sum)

data SfCreds = SfCreds
  { username :: Text,
    password :: Text,
    clientId :: Text,
    clientSecret :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SfCreds

instance FromJSON SfCreds

encodeCreds :: SfCreds -> [FormParam]
encodeCreds SfCreds {..} =
  [ ("username" := username),
    ("password" := password),
    ("grant_type" := ("password" :: Text)),
    ("client_id" := clientId),
    ("client_secret" := clientSecret)
  ]

