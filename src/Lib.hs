{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Lens (preview, set, view, (&), (.~), (?~), (^..), (^?))
import Data.Aeson (FromJSON, ToJSON, Value (..), decodeFileStrict')
import Data.Aeson.Lens (AsJSON (..), AsValue (..), key)
import Data.Map.Monoidal.Strict (MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MonoidalMap
import Data.Maybe (catMaybes, fromMaybe)
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
    auth,
    defaults,
    getWith,
    manager,
    oauth2Bearer,
    param,
    postWith,
    responseBody,
  )
import OpenSSL.Session (context)
import Streaming (Of, Stream)

-- salesforce interaction, pls comment everything

type Token = Text

queryRoot :: String -> String
queryRoot x = "https://globalaccess.my.salesforce.com/services/" <> x

queryServices :: String -> String
queryServices x = queryRoot $ "data/v53.0/" <> x

data SfCreds = SfCreds
  { username :: Text,
    password :: Text,
    client_id :: Text,
    client_secret :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON SfCreds

encodeCreds :: SfCreds -> [FormParam]
encodeCreds SfCreds {..} =
  [ ("username" := username),
    ("password" := password),
    ("grant_type" := ("password" :: Text)),
    ("client_id" := client_id),
    ("client_secret" := client_secret)
  ]

queryToken :: String
queryToken = queryRoot "oauth2/token"

-- we are inlining a *post* here, it would be nice to factor
-- it out for reuse, see *get* polymorphic below
getToken :: Options -> IO Token
getToken opts = do
  Just creds <- decodeFileStrict' "creds.json"
  response <-
    fmap (view responseBody) $
      withOpenSSL $
        postWith opts queryToken $ encodeCreds creds
  case response ^? key "access_token" . _String of
    Nothing -> error "no token"
    Just txt -> pure txt

getAny :: Options -> String -> IO Value
getAny opts q =
  fmap (fromMaybe Null . preview (responseBody . _JSON)) $
    withOpenSSL $ getWith opts q

data Result a = Done [a] | NotDone [a] | Failed Value

parseResult :: (FromJSON a, ToJSON a) => Value -> Result a
parseResult response =
  let result = do
        done <- response ^? key "done" . _Bool
        xs <- pure $ response ^.. key "records" . _Array . traverse . _JSON
        pure $ if done then Done xs else NotDone xs
   in fromMaybe (Failed response) result

selectAny :: (FromJSON a, ToJSON a) => Options -> Text -> IO (Result a)
selectAny opts q = do
  let opts' = set (param "q") [q] opts
  fmap parseResult $ getAny opts' $ queryServices "query"

-- nikola this is a bit more functional, you see we produce a function to execute queries against salesforce
--
-- tobias, see that as the function is polytyped (the output is polymorphic) we need a newtype
-- incidentally this is somehow necessay with ghc 9 and related to the deep thing
newtype Query = Query {runQ :: forall a. (FromJSON a, ToJSON a) => Text -> IO (Result a)}

querySalesforceX :: (Options -> IO w) -> IO w
querySalesforceX action = do
  let opts = defaults & manager .~ Left (opensslManagerSettings context)
  token <- getToken opts
  let opts' = opts & auth ?~ oauth2Bearer (Text.encodeUtf8 token)
  action opts'

querySalesforce :: IO Query
querySalesforce = querySalesforceX $
  \opts' -> pure $
    Query $ \qtext -> selectAny opts' qtext

-- ########## EXAMPLE

-- pls notice that querySalesforce is called only once
example :: IO ()
example = do
  Query q <- querySalesforce
  result <- q "select Name from Account limit 5"
  case result of
    Done xs -> mapM_ (print @Value) xs
    NotDone _ -> error "unhandled"
    Failed v -> print v
  result' <- q queryRamsExample
  case result' of
    Done xs -> mapM_ (print @Value) xs
    NotDone _ -> error "unhandled"
    Failed v -> print v

queryRamsExample :: Text
queryRamsExample =
  "SELECT \
  \Account__r.Name,\
  \Month__c,\
  \Product_Name__c,\
  \Usage__c,\
  \Year__c\
  \ FROM Monthly_Usage__c\
  \ WHERE Product_Name__c LIKE '%RAM%'\
  \ ORDER BY YearMonth__c DESC NULLS FIRST limit 50"

-------- exercise: implement undefineds

type Usage = Double

type Account = Text

parseRamConsumption :: Value -> Maybe (Account, Sum Usage)
parseRamConsumption = error "implement parseRamConsumption"

type Usages = MonoidalMap Account (Sum Usage)

usages :: [Maybe (Account, Sum Usage)] -> Usages
usages = MonoidalMap.fromList . catMaybes

-- sorted by biggest
ranking :: Usages -> [(Usage, Account)]
ranking = error "implement ranking"

queryRams :: Text
queryRams = error "implement queryRams"

exercise :: IO ()
exercise = do
  Query q <- querySalesforce
  result' <- q queryRams
  case result' of
    Done xs -> do
      let us = usages $ parseRamConsumption <$> xs
      mapM_ print $ ranking us
    NotDone _ -> error "unhandled"
    Failed v -> print v

-- more exercises, longer term

-- ############ nikola
--
-- select queries in SOQL have some fixed syntax! ... select _columns_ from _table_ where  _conditions_ ...

-- a) create a simple API to write them as an haskell data type ...
data Select = Select

-- add some types to Select so that you can

-- b) write a function
renderSelect :: Select -> Text
renderSelect = error "implement renderSelect"

-- that given a value of select creates a valid query for SOQL

-- ############## tobias
--
-- as you have noticed `Result a` is weird !

-- a) if the result has more than 10000 records it will be chunked and so the result will be UnDone  prove it pls

-- b) UnDone constructor makes not much sense, how do we get the rest out ?
--     check out the result again to see what I did not peek in `parseResult` that will represent the next chunk
--     and expand UnDone arguments with a type that hold how to continue the execution of the query

-- c) modify selectAny into selectAnyS  to take care of UnDones and recurse in that case and produce instead a `Stream (Of a) IO Value`
--    where Value is from  the 'Failed' constructor

type SFStream a = Stream (Of a) IO Value

selectAnyS :: (FromJSON a, ToJSON a) => Options -> Text -> SFStream a
selectAnyS = undefined

-- as in the original but these queries will return streams
newtype QueryS = QueryS {runQS :: forall a. (FromJSON a, ToJSON a) => Text -> SFStream a}

querySalesforceS :: IO QueryS
querySalesforceS = querySalesforceX $ \opts' ->
  pure $ QueryS $ \qtext -> selectAnyS opts' qtext

-- d) implement the Usages computation using the stream and using Fold package, 
-- d1) use `purely` from that package and `fold` from Streaming
-- d2) use directly `foldMap` from Streaming
foldUsages :: SFStream Value -> IO (Usages, Value)
foldUsages = undefined

exerciseS :: IO ()
exerciseS = do
  QueryS q <- querySalesforceS
  (us, rest) <- foldUsages $ q queryRams
  mapM_ print $ ranking us
  print rest