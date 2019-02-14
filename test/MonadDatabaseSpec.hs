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
import           WebServer.UrlParser.Pagination
import           Control.Monad.Except
import           Control.Exception              ( bracket )
import           Helpers

mockApp :: IO Application
mockApp = pure $ newsServer mockRoutes runMockHandler

runMockHandler :: Request -> DynamicPathsMap -> MockHandler -> IO Response
runMockHandler req dpMap handler = do
  conf     <- C.loadConfig
  maxLimit <- Limit <$> C.get conf "pagination.max_limit"
  let mockDb = undefined
  res <- bracket (DC.connect conf) PSQL.close
    $ \conn -> pure $ runMock mockDb maxLimit dpMap req conn handler
  either (const serverErrorResponse) pure res

spec :: Spec
spec = with mockApp $ describe "GET /" $ do
  it "responds with 200" $ get "/" `shouldRespondWith` 200
