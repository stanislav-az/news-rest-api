{-# LANGUAGE OverloadedStrings #-}

module WebServer.Router where

import qualified Data.Text                     as T
import qualified Data.ByteString               as BS
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           Helpers
import           WebServer.Error
import           Network.Wai
import           Network.HTTP.Types
import           WebServer.HandlerClass
import           WebServer.HandlerMonad
import           Control.Exception              ( bracket )
import           Control.Monad.Except
import           WebServer.UrlParser.Pagination

data Route = PathRoute T.Text Route | DynamicRoute T.Text Route | MethodRoute BS.ByteString

checkout
  :: DynamicPathsMap
  -> Route
  -> [T.Text]
  -> BS.ByteString
  -> (Bool, DynamicPathsMap)
checkout dpMap (MethodRoute x) [] method | x == method = (True, dpMap)
                                         | otherwise   = (False, [])
checkout dpMap (MethodRoute x) [""] method | x == method = (True, dpMap)
                                           | otherwise   = (False, [])
checkout dpMap (MethodRoute x) xs method = (False, [])
checkout dpMap route           [] _      = (False, [])
checkout dpMap (PathRoute s route) (x : xs) method
  | x == s    = checkout dpMap route xs method
  | otherwise = (False, [])
checkout dpMap (DynamicRoute s route) (x : xs) method =
  checkout ((s, x) : dpMap) route xs method

route :: [(Route, Handler)] -> Request -> IO Response
route [] req = notFoundResponse
route (h : hs) req | isCorrect = liftIO runHAndCatchE
                   | otherwise = route hs req
 where
  (isCorrect, dpMap) = checkout [] currentRoute path method
  currentRoute       = fst h
  handler            = snd h
  path               = pathInfo req
  method             = requestMethod req
  runHAndCatchE      = do
    conf     <- C.loadConfig
    maxLimit <- Limit <$> C.get conf "pagination.max_limit"
    res      <- bracket (DC.connect conf) PSQL.close $ \conn ->
      runHandler maxLimit dpMap req conn $ catchError handler manageHandlerError
    either (\e -> (logError $ texify e) >> serverErrorResponse) pure res
