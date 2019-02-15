{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MonadDatabaseSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher
import           MockRoutes
import           WebServer.Application
import           WebServer.HandlerMonad
import           WebServer.HandlerClass
import           Network.Wai
import           MockMonad
import qualified Config                        as C
import qualified Database.Connection           as DC
import qualified Database.PostgreSQL.Simple    as PSQL
import           WebServer.UrlParser.Pagination
import           Control.Monad.Except
import           Control.Exception              ( bracket )
import           Helpers
import qualified Data.Map.Strict               as M
import           Database.Models.User
import           Serializer.User                ( userToResponse )
import           Data.Aeson
import           WebServer.Error

mockApp :: IO Application
mockApp = pure $ newsServer mockRoutes runMockHandler

database :: MockDB
database = DB (M.fromAscList usersMap) (M.fromAscList authorsMap)
 where
  usersMap   = zip [1 ..] users
  authorsMap = []

users :: [User]
users =
  [ User { userId          = 1
         , userName        = "User1"
         , userSurname     = "Userovich1"
         , userAvatar      = "http1"
         , userDateCreated = sometime
         , userIsAdmin     = False
         }
  , User { userId          = 2
         , userName        = "User2"
         , userSurname     = "Userovich2"
         , userAvatar      = "http2"
         , userDateCreated = sometime
         , userIsAdmin     = False
         }
  , User { userId          = 3
         , userName        = "User3"
         , userSurname     = "Userovich3"
         , userAvatar      = "http3"
         , userDateCreated = sometime
         , userIsAdmin     = False
         }
  , User { userId          = 4
         , userName        = "User4"
         , userSurname     = "Userovich4"
         , userAvatar      = "http4"
         , userDateCreated = sometime
         , userIsAdmin     = False
         }
  , User { userId          = 5
         , userName        = "Admin"
         , userSurname     = "Adminovich"
         , userAvatar      = "http5"
         , userDateCreated = sometime
         , userIsAdmin     = True
         }
  ]

userRaw :: UserRaw
userRaw = UserRaw "Newuser" "Userovich" "http"

userRawUser :: User
userRawUser = User { userId          = 1
                   , userName        = "Newuser"
                   , userSurname     = "Userovich"
                   , userAvatar      = "http"
                   , userDateCreated = sometime
                   , userIsAdmin     = False
                   }

instance ToJSON UserRaw where
  toJSON UserRaw {..} = object
    [ "name" .= userRawName
    , "surname" .= userRawSurname
    , "avatar" .= userRawAvatar
    ]

runMockHandler :: Request -> DynamicPathsMap -> MockHandler -> IO Response
runMockHandler req dpMap handler = do
  conf <- C.loadConfig
  body <- strictRequestBody req
  res  <- bracket (DC.connect conf) PSQL.close $ \conn ->
    pure $ runMock database body (Limit 3) dpMap req conn $ catchError
      handler
      manageHandlerError
  either (\e -> (logError $ texify e) >> serverErrorResponse) pure res

spec :: Spec
spec = with mockApp $ do
  describe "GET /" $ it "responds with 200" $ get "/" `shouldRespondWith` 200

  describe "GET /api/users/" $ do
    it "responds with 200" $ get "/api/users/" `shouldRespondWith` 200
    it "responds with correct JSON on calling with default limit and offset"
      $                   get "/api/users/"
      `shouldRespondWith` 200
                            { matchBody =
                              bodyEquals
                                (encode $ userToResponse <$> take 3 users)
                            }
    it "responds with correct JSON on calling with custom limit and offset"
      $                   get "/api/users?limit=2&offset=1"
      `shouldRespondWith` 200
                            { matchBody =
                              bodyEquals
                                (   encode
                                $   userToResponse
                                <$> (take 2 $ drop 1 users)
                                )
                            }

  describe "POST /api/users/" $ do
    it "responds with 200"
      $                   post "/api/users/" (encode userRaw)
      `shouldRespondWith` 200
    it "responds with correct JSON"
      $                   post "/api/users/" (encode userRaw)
      `shouldRespondWith` 200
                            { matchBody =
                              bodyEquals (encode $ userToResponse userRawUser)
                            }

  describe "DELETE /api/users/" $ do
    it "responds with 404 without authorization header"
      $                   delete "/api/users/1"
      `shouldRespondWith` 404
    it "responds with 404 with incorrect authorization header"
      $ request "DELETE" "/api/users/1" [("Authorization", "1")] ""
      `shouldRespondWith` 404
    it "responds with 204 with correct authorization header"
      $ request "DELETE" "/api/users/1" [("Authorization", "5")] ""
      `shouldRespondWith` 204
    it "responds with 422 with incorrect user id"
      $ request "DELETE" "/api/users/66" [("Authorization", "5")] ""
      `shouldRespondWith` 422
