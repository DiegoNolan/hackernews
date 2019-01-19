{-# LANGUAGE CPP                        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews
-- Copyright   : (c) David Johnson, 2014-2016
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Haskell port of <https://github.com/HackerNews/API>
--
------------------------------------------------------------------------------
module Web.HackerNews
  ( -- * Hacker News API
    HackerNewsAPI
   -- * Custom combinators
  , HackerCapture
   -- * API functions
  , getItem
  , getUser
  , getMaxItem
  , getTopStories
  , getNewStories
  , getBestStories
  , getAskStories
  , getShowStories
  , getJobStories
  , getUpdates
  -- * Core Types
  , Item        (..)
  , User        (..)
  , Updates     (..)
  , MaxItem     (..)
  , TopStories  (..)
  , NewStories  (..)
  , BestStories (..)
  , AskStories  (..)
  , ShowStories (..)
  , JobStories  (..)
  --- * Supporting Types
  , UserId      (..)
  , ItemId      (..)
  , Deleted     (..)
  , ItemType    (..)
  , Time        (..)
  , ItemText    (..)
  , Dead        (..)
  , Parent      (..)
  , Kids        (..)
  , URL         (..)
  , Score       (..)
  , Title       (..)
  , Parts       (..)
  , Descendants (..)
  , Delay       (..)
  , Created     (..)
  , Karma       (..)
  , About       (..)
  , Submitted   (..)
  --- * Error handling
  , HackerNewsError (..)
  ) where

import           Data.Bifunctor
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import qualified Data.Text                  as T
import           Network.HTTP.Client        (Manager)
import           Network.HTTP.Types.Status
import           Servant.API
import           Servant.Client
import           Servant.Client.Core.Internal.Request (appendToPath)

import           Web.HackerNews.Types

-- | HackerNews API
type HackerNewsAPI =
       "item" :> HackerCapture ItemId :> Get '[JSON] Item
  :<|> "user" :> HackerCapture UserId :> Get '[JSON] User
  :<|> "maxitem.json" :> Get '[JSON] MaxItem
  :<|> "topstories.json" :> Get '[JSON] TopStories
  :<|> "newstories.json" :> Get '[JSON] NewStories
  :<|> "beststories.json" :> Get '[JSON] BestStories
  :<|> "askstories.json" :> Get '[JSON] AskStories
  :<|> "showstories.json" :> Get '[JSON] ShowStories
  :<|> "jobstories.json" :> Get '[JSON] JobStories
  :<|> "updates.json" :> Get '[JSON] Updates

-- | Custom combinator for appending '.json' to `Item` query
data HackerCapture (a :: *)

-- | Custom combinator `HasClient` instance
instance (ToHttpApiData a, HasClient m api) => HasClient m (HackerCapture a :> api) where
  type Client m (HackerCapture a :> api) = a -> Client m api
  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api)
      (appendToPath p req)
    where
      p = toUrlPiece val <> ".json"

  hoistClientMonad pm _ f cl = \val ->
    hoistClientMonad pm (Proxy :: Proxy api) f (cl val)

-- | HN `BaseURL`
hackerNewsURL :: BaseUrl
hackerNewsURL = BaseUrl Https "hacker-news.firebaseio.com" 443 "/v0"

-- | Convert ServantError to HackerNewsError
toError :: Either ServantError ok -> Either HackerNewsError ok
toError = first go
  where
    go :: ServantError -> HackerNewsError
    go (FailureResponse resp) =
      FailureResponseError (statusCode (responseStatusCode resp))
                            ("") (cs (responseBody resp))
    go (DecodeFailure err resp) =
      DecodeFailureError (cs err) (cs (responseBody resp))
    go (UnsupportedContentType _ resp) =
      UnsupportedContentTypeError (cs (responseBody resp))
    go (InvalidContentTypeHeader resp) =
      InvalidContentTypeHeaderError "" (cs (responseBody resp))
    go (ConnectionError ex) =
      HNConnectionError $ cs (show ex)

-- | Retrieve `Item`
getItem :: Manager -> ItemId -> IO (Either HackerNewsError Item)
getItem mgr itemId =
  toError <$> do
    runClientM
      (getItem' itemId)
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `User`
getUser :: Manager -> UserId -> IO (Either HackerNewsError User)
getUser mgr userId =
  toError <$> do
    runClientM
      (getUser' userId)
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `MaxItem`
getMaxItem :: Manager -> IO (Either HackerNewsError MaxItem)
getMaxItem mgr =
  toError <$> do
    runClientM
      getMaxItem'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `TopStories`
getTopStories :: Manager -> IO (Either HackerNewsError TopStories)
getTopStories mgr =
  toError <$> do
    runClientM
      getTopStories'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `NewStories`
getNewStories :: Manager -> IO (Either HackerNewsError NewStories)
getNewStories mgr =
  toError <$> do
    runClientM
      getNewStories'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `BestStories`
getBestStories :: Manager -> IO (Either HackerNewsError BestStories)
getBestStories mgr =
  toError <$> do
    runClientM
      getBestStories'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `AskStories`
getAskStories :: Manager -> IO (Either HackerNewsError AskStories)
getAskStories mgr =
  toError <$> do
    runClientM
      getAskStories'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `ShowStories`
getShowStories :: Manager -> IO (Either HackerNewsError ShowStories)
getShowStories mgr =
  toError <$> do
    runClientM
      getShowStories'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `JobStories`
getJobStories :: Manager -> IO (Either HackerNewsError JobStories)
getJobStories mgr =
  toError <$> do
    runClientM
      getJobStories'
      (mkClientEnv mgr hackerNewsURL)

-- | Retrieve `Updates`
getUpdates :: Manager -> IO (Either HackerNewsError Updates)
getUpdates mgr =
  toError <$> do
    runClientM
      getUpdates'
      (mkClientEnv mgr hackerNewsURL)

getItem' :: ItemId -> ClientM Item
getUser' :: UserId -> ClientM User
getMaxItem' :: ClientM MaxItem
getTopStories' :: ClientM TopStories
getNewStories' :: ClientM NewStories
getBestStories' :: ClientM BestStories
getAskStories' :: ClientM AskStories
getShowStories' :: ClientM ShowStories
getJobStories' :: ClientM JobStories
getUpdates' ::  ClientM Updates

getItem'
  :<|> getUser'
  :<|> getMaxItem'
  :<|> getTopStories'
  :<|> getNewStories'
  :<|> getBestStories'
  :<|> getAskStories'
  :<|> getShowStories'
  :<|> getJobStories'
  :<|> getUpdates' = client (Proxy :: Proxy HackerNewsAPI)

