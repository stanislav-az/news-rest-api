{-# LANGUAGE OverloadedStrings #-}

module Middlewares where

import           WebServer.MonadHandler
import           WebServer.Database
import           Control.Monad.Reader
import qualified Database.PostgreSQL.Simple    as PSQL
import           Network.Wai
import           Database.Queries.User
import           Database.Models.User
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS
import           Text.Read
import           Helpers
import qualified Network.HTTP.Types            as HTTP

data Permission = Admin
    | Owner (PSQL.Connection -> User -> Integer -> IO Bool)

checkPermission :: Permission -> Handler -> Handler
checkPermission Admin handler = do
  req  <- asks hRequest
  conn <- asks hConnection
  user <- liftIO $ getUser conn req
  checkUserAdmin user handler
checkPermission (Owner f) handler = do
  req  <- asks hRequest
  conn <- asks hConnection
  user <- liftIO $ getUser conn req
  checkUserOwner user f handler

checkUserAdmin :: Maybe User -> Handler -> Handler
checkUserAdmin Nothing     _       = hasNoPermissionResponse
checkUserAdmin (Just user) handler = handler

checkUserOwner
  :: Maybe User
  -> (PSQL.Connection -> User -> Integer -> IO Bool)
  -> Handler
  -> Handler
checkUserOwner Nothing _ _ = hasNoPermissionResponse
checkUserOwner (Just user) f handler
  | userIsAdmin user = handler
  | otherwise = do
    dpMap <- asks hDynamicPathsMap
    conn  <- asks hConnection
    let checkOwner = maybe (pure False) (f conn user) objId
        objId      = getObjectId dpMap
    isOwner <- liftIO checkOwner
    if isOwner then handler else hasNoPermissionResponse

getObjectId :: DynamicPathsMap -> Maybe Integer
getObjectId dpMap = objId >>= eitherToMaybe
  where objId = textToInteger <$> lookup "id" dpMap

getAuthHeader :: Request -> Maybe Integer
getAuthHeader req =
  let headers = requestHeaders req
      bsId    = lookup "Authorization" headers
  in  bsId >>= (readMaybe . BS.unpack)

getUser :: PSQL.Connection -> Request -> IO (Maybe User)
getUser conn req = maybe (pure Nothing) (selectById conn) $ getAuthHeader req

hasNoPermissionResponse :: (Applicative m) => m Response
hasNoPermissionResponse =
  pure $ responseLBS HTTP.status404 [("Content-Type", "plain/text")] "Not Found"
