{-# LANGUAGE OverloadedStrings #-}

module Middlewares where

import           Handlers
import           Network.Wai
import           Database.Queries.User
import           Database.Models.User
import qualified Data.Text                     as T
import qualified Data.ByteString.Char8         as BS
import           Text.Read
import           Helpers
import           Network.HTTP.Types

data Permission = Admin
    | Owner (User -> Integer -> IO Bool)
    | Regular

checkPermission :: Permission -> Handler -> Handler
checkPermission Admin handler dpMap req = do
  user <- getUser req
  checkUserAdmin user handler dpMap req
checkPermission (Owner f) handler dpMap req = do
  user <- getUser req
  checkUserOwner user f handler dpMap req

checkUserAdmin :: Maybe User -> Handler -> Handler
checkUserAdmin Nothing     _       _     _   = hasNoPermissionResponse
checkUserAdmin (Just user) handler dpMap req = handler dpMap req

checkUserOwner
  :: Maybe User -> (User -> Integer -> IO Bool) -> Handler -> Handler
checkUserOwner Nothing _ _ _ _ = hasNoPermissionResponse
checkUserOwner (Just user) f handler dpMap req
  | userIsAdmin user = handler dpMap req
  | otherwise = do
    isOwner <- checkOwner
    if isOwner then handler dpMap req else hasNoPermissionResponse
 where
  checkOwner = maybe (pure False) (f user) objId
  objId      = getObjectId dpMap

getObjectId :: [(T.Text, T.Text)] -> Maybe Integer
getObjectId dpMap = objId >>= eitherToMaybe
  where objId = textToInteger <$> lookup "id" dpMap

getAuthHeader :: Request -> Maybe Integer
getAuthHeader req =
  let headers = requestHeaders req
      bsId    = lookup "Authorization" headers
  in  bsId >>= (readMaybe . BS.unpack)

getUser :: Request -> IO (Maybe User)
getUser req = maybe (pure Nothing) getUserById $ getAuthHeader req

hasNoPermissionResponse :: IO Response
hasNoPermissionResponse = pure $
  responseLBS status404 [("Content-Type", "plain/text")] "Not Found"
