{-# LANGUAGE OverloadedStrings #-}

module MonadDatabaseSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Hspec.Wai
import           MockRoutes
import           WebServer.Application
import           WebServer.HandlerMonad
import           Network.Wai
import           MockMonad
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           Data.ByteString.Lazy.Char8    as L
import           WebServer.UrlParser.Pagination
import           Control.Monad.Except
import           Control.Exception              ( bracket )
import           Helpers
import qualified Data.Map.Strict               as M

mockApp :: IO Application
mockApp = pure $ newsServer mockRoutes runMockHandler

database :: MockDB
database = DB (M.fromAscList users) (M.fromAscList authors)
  where
    users = []
    authors = []

runMockHandler :: Request -> DynamicPathsMap -> MockHandler -> IO Response
runMockHandler req dpMap handler = do
  conf     <- C.loadConfig
  maxLimit <- Limit <$> C.get conf "pagination.max_limit"
  body     <- strictRequestBody req
  res      <- bracket (DC.connect conf) PSQL.close
    $ \conn -> pure $ runMock database body maxLimit dpMap req conn handler
  either (const serverErrorResponse) pure res

spec :: Spec
spec = with mockApp $ do
  describe "GET /" $ it "responds with 200" $ get "/" `shouldRespondWith` 200
  describe "GET /api/users/" $ do
    it "responds with 200" $ get "/api/users/" `shouldRespondWith` 200
    it "responds with correct JSON" $ get "/api/users/" `shouldRespondWith` "[]"

