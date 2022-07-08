{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Lib where

import Network.Wreq ( getWith, defaults, manager, Response, responseBody, post, postWith, FormParam (..), Options, auth, oauth2Bearer )
import OpenSSL.Session (context)
import Network.HTTP.Client.OpenSSL
    ( withOpenSSL, opensslManagerSettings )
import Control.Lens ( (&), (.~), view, (^?), to, (?~) )
import Data.ByteString.Lazy ( ByteString )
import Data.Aeson (decode, Value, FromJSON, ToJSON)
import Data.Aeson (decode, Value, decodeFileStrict')
import Data.Text (Text)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Semigroup (Sum (..))
import qualified Data.Map.Monoidal.Strict as MonoidalMap
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Data.Aeson.Lens ( key, AsValue(_String) ) 
import qualified Data.Text.Encoding as Text
 

type Result = MonoidalMap Account (Sum Usage)

type Usage = Double 
type Account = Text 

type Record = Map Text Value

type Token = Text 

oauthApi :: String
oauthApi = "services/oauth2/token"

getToken :: Options -> IO Token
getToken opts = do
    Just creds <- decodeFileStrict' "creds.json"
    response <- fmap (view responseBody) $ withOpenSSL 
                    $ postWith opts oauthApi $ encodeCreds creds 
    case response ^? key "access_token" . _String  of
            Nothing -> error "no token"
            Just txt -> pure txt 

getAny :: FromJSON a => Options ->  String -> IO (Maybe a)
getAny opts  q  = decode . view responseBody <$> 
    do withOpenSSL $ getWith opts q 
    
querySalesforce :: String -> IO (Maybe Value) -- IO (Maybe [Record]) -- 
querySalesforce query = do
    let opts = defaults & manager .~ Left (opensslManagerSettings context)
    token <- getToken opts
    let opts' = opts & auth ?~ oauth2Bearer (Text.encodeUtf8 token) 
    getAny opts'  "asdnsadnkjenk3"


 


computeRanking :: [Record] -> Result 
computeRanking xs = MonoidalMap.fromList undefined -- fmap (fmap Sum)  

data SfCreds = SfCreds
  { username :: Text
  , password :: Text
  , clientId :: Text
  , clientSecret :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON SfCreds 
instance FromJSON SfCreds 

encodeCreds ::  SfCreds -> [FormParam]
encodeCreds SfCreds {..} =
  [ ("username" := username)
  , ("password":= password)
  , ("grant_type" := ("password" :: Text))
  , ("client_id" :=  clientId)
  , ("client_secret" := clientSecret)
  ]


-- clientEnv :: BaseUrl -> IO ClientEnv
-- clientEnv bu = do
--   manager <- newManager tlsManagerSettings {managerResponseTimeout = responseTimeoutMicro $ 60 * 1_000_000}
--   pure $ mkClientEnv manager bu

-- runEnvClientM :: BaseUrl -> ClientM a -> IO a
-- runEnvClientM bu c = do
--   ce <- clientEnv bu
--   r <- runClientM c ce
--   case r of
--     Left e -> throwM e
--     Right x -> pure x

-- callSFY :: MonadIO m => FilePath -> m (CallSFT m)
-- callSFY c = liftIO (decodeFileThrow $ encodeString c) >>= callSFConf

-- callSFConf :: MonadIO m => SfConf -> m (CallSFT m)
-- callSFConf (SfConf bu cs) = liftIO $ do
--   pbu <- parseBaseUrl $ toS bu
--   tk <- runEnvClientM pbu $ getToken $ mkCreds cs
--   pure $ CallSFT $ \f -> liftIO $ runEnvClientM pbu $ runReaderT f tk

-- withSF :: MonadIO m => (FilePath -> m (CallSFT m)) -> FilePath -> WithSF b -> m b
-- withSF call fp f = do
--   CallSFT sf <- call fp
--   sf f

-- withSFY :: MonadIO m => FilePath -> WithSF b -> m b
-- withSFY = withSF callSFY

-- withToken :: (Maybe Token -> ClientM a) -> WithSF a
-- withToken f = ask >>= lift . f . Just


-- type LoginApi =
--   OauthApi (ReqBody '[FormUrlEncoded] [(Text, Text)] :> Post '[JSON] Value)

-- loginP :: Proxy LoginApi
-- loginP = Proxy


-- data SfConf = SfConf
--   { sfUrl :: Text
--   , sfCreds :: SfCreds
--   }
--   deriving (Show, Eq, Generic)
--   deriving
--     (FromJSON)
--     via CustomJSON '[FieldLabelModifier '[CamelToSnake, StripPrefix "sf_"]] SfConf

