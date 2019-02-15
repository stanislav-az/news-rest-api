{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Middlewares where

import           WebServer.HandlerMonad
import           WebServer.HandlerClass
import           WebServer.MonadDatabase
import           WebServer.Database
import           Control.Monad.Reader
import           Control.Monad.Except
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
  :: (MonadReader HandlerEnv m, PersistentUser m, MonadError HandlerError m)
  => Permission m
  -> m a
  -> m a
checkPermission Admin handler = do
  req  <- asks hRequest
  user <- getUser req
  checkUserAdmin user handler
checkPermission (Owner f) handler = do
  req  <- asks hRequest
  user <- getUser req
  checkUserOwner user f handler

checkUserAdmin :: MonadError HandlerError m => Either a User -> m a1 -> m a1
checkUserAdmin (Left _) _ = throwError Forbidden
checkUserAdmin (Right user) handler | userIsAdmin user = handler
                                    | otherwise        = throwError Forbidden

checkUserOwner
  :: (MonadError HandlerError m, MonadReader HandlerEnv m)
  => Either a User
  -> (User -> Integer -> m Bool)
  -> m a1
  -> m a1
checkUserOwner (Left _) _ _ = throwError Forbidden
checkUserOwner (Right user) f handler
  | userIsAdmin user = handler
  | otherwise = do
    dpMap <- asks hDynamicPathsMap
    conn  <- asks hConnection
    let checkOwner = maybe (pure False) (f user) objId
        objId      = getObjectId dpMap
    isOwner <- checkOwner
    if isOwner then handler else throwError Forbidden

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

