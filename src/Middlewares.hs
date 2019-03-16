{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Middlewares where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BS (unpack)
import Database.Models.User (User(..))
import Helpers (eitherToMaybe, textToInteger)
import qualified Network.Wai as W (Request(..), requestHeaders)
import qualified Text.Read as R (readMaybe)
import WebServer.HandlerMonad
  ( DynamicPathsMap(..)
  , HandlerEnv(..)
  , HandlerError(..)
  )
import WebServer.MonadDatabase (PersistentUser(..))

data Permission m
  = Admin
  | Owner (User -> Integer -> m Bool)

checkPermission ::
     (MonadReader HandlerEnv m, PersistentUser m, MonadError HandlerError m)
  => Permission m
  -> m a
  -> m a
checkPermission Admin handler = do
  req <- asks hRequest
  user <- getUser req
  checkUserAdmin user handler
checkPermission (Owner f) handler = do
  req <- asks hRequest
  user <- getUser req
  checkUserOwner user f handler

checkUserAdmin :: MonadError HandlerError m => Either a User -> m a1 -> m a1
checkUserAdmin (Left _) _ = throwError Forbidden
checkUserAdmin (Right user) handler
  | userIsAdmin user = handler
  | otherwise = throwError Forbidden

checkUserOwner ::
     (MonadError HandlerError m, MonadReader HandlerEnv m)
  => Either a User
  -> (User -> Integer -> m Bool)
  -> m a1
  -> m a1
checkUserOwner (Left _) _ _ = throwError Forbidden
checkUserOwner (Right user) f handler
  | userIsAdmin user = handler
  | otherwise = do
    dpMap <- asks hDynamicPathsMap
    conn <- asks hConnection
    let checkOwner = maybe (pure False) (f user) objId
        objId = getObjectId dpMap
    isOwner <- checkOwner
    if isOwner
      then handler
      else throwError Forbidden

getObjectId :: DynamicPathsMap -> Maybe Integer
getObjectId dpMap = objId >>= eitherToMaybe
  where
    objId = textToInteger <$> lookup "id" dpMap

getAuthHeader :: W.Request -> Maybe Integer
getAuthHeader req =
  let headers = W.requestHeaders req
      bsId = lookup "Authorization" headers
   in bsId >>= (R.readMaybe . BS.unpack)

getUser :: PersistentUser f => W.Request -> f (Either String User)
getUser req =
  maybe (pure $ Left "No authorization header") selectUserById $
  getAuthHeader req
