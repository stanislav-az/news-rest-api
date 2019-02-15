{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Middlewares where

import           WebServer.HandlerMonad
import           WebServer.HandlerClass
import           WebServer.MonadDatabase
import           WebServer.Database
import           Control.Monad.Reader
import qualified Database.PostgreSQL.Simple    as PSQL
import           Network.Wai
import           Database.Models.User
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS
import           Text.Read
import           Helpers
import qualified Network.HTTP.Types            as HTTP

data Permission m = Admin
    | Owner (User -> Integer -> m Bool)

checkPermission
  :: (MonadReader HandlerEnv m, PersistentUser m, MonadHTTP m)
  => Permission m
  -> m Response
  -> m Response
checkPermission Admin handler = do
  req  <- asks hRequest
  user <- getUser req
  checkUserAdmin user handler
checkPermission (Owner f) handler = do
  req  <- asks hRequest
  user <- getUser req
  checkUserOwner user f handler

checkUserAdmin :: MonadHTTP m => Either a User -> m Response -> m Response
checkUserAdmin (Left _) _ = notFoundResponse
checkUserAdmin (Right user) handler | userIsAdmin user = handler
                                    | otherwise        = notFoundResponse

checkUserOwner
  :: (MonadHTTP m, MonadReader HandlerEnv m)
  => Either a User
  -> (User -> Integer -> m Bool)
  -> m Response
  -> m Response
checkUserOwner (Left _) _ _ = notFoundResponse
checkUserOwner (Right user) f handler
  | userIsAdmin user = handler
  | otherwise = do
    dpMap <- asks hDynamicPathsMap
    conn  <- asks hConnection
    let checkOwner = maybe (pure False) (f user) objId
        objId      = getObjectId dpMap
    isOwner <- checkOwner
    if isOwner then handler else notFoundResponse

getObjectId :: DynamicPathsMap -> Maybe Integer
getObjectId dpMap = objId >>= eitherToMaybe
  where objId = textToInteger <$> lookup "id" dpMap

getAuthHeader :: Request -> Maybe Integer
getAuthHeader req =
  let headers = requestHeaders req
      bsId    = lookup "Authorization" headers
  in  bsId >>= (readMaybe . BS.unpack)

getUser :: PersistentUser f => Request -> f (Either String User)
getUser req = maybe (pure $ Left "No authorization header") selectUserById
  $ getAuthHeader req

