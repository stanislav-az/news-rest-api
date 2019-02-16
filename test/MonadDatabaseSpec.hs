{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Database.Models.Author
import           Serializer.User                ( userToResponse )
import           Serializer.Author              ( authorToResponse )
import           Data.Aeson
import           WebServer.Error

mockApp :: IO Application
mockApp = pure $ newsServer mockRoutes runMockHandler

database :: MockDB
database = DB (mapify users) (mapify authors)
  where mapify = M.fromAscList . zip [1 ..]

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

authors :: [(Author, User)]
authors =
  [ ( Author { authorId          = 1
             , authorUserId      = 1
             , authorDescription = "Delicate soul1"
             }
    , User { userId          = 1
           , userName        = "Author1"
           , userSurname     = "Authorovich1"
           , userAvatar      = "http1"
           , userDateCreated = sometime
           , userIsAdmin     = False
           }
    )
  , ( Author { authorId          = 2
             , authorUserId      = 2
             , authorDescription = "Delicate soul2"
             }
    , User { userId          = 2
           , userName        = "Author2"
           , userSurname     = "Authorovich2"
           , userAvatar      = "http2"
           , userDateCreated = sometime
           , userIsAdmin     = False
           }
    )
  , ( Author { authorId          = 3
             , authorUserId      = 3
             , authorDescription = "Delicate soul3"
             }
    , User { userId          = 3
           , userName        = "Author3"
           , userSurname     = "Authorovich3"
           , userAvatar      = "http3"
           , userDateCreated = sometime
           , userIsAdmin     = False
           }
    )
  , ( Author { authorId          = 4
             , authorUserId      = 4
             , authorDescription = "Delicate soul4"
             }
    , User { userId          = 4
           , userName        = "Author4"
           , userSurname     = "Authorovich4"
           , userAvatar      = "http4"
           , userDateCreated = sometime
           , userIsAdmin     = False
           }
    )
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

authorRaw :: (AuthorRaw, UserRaw)
authorRaw =
  ( AuthorRaw { authorRawDescription = "New here" }
  , UserRaw { userRawName    = "Newauthor"
            , userRawSurname = "Authorovich"
            , userRawAvatar  = "http"
            }
  )

authorRawAuthor :: (Author, User)
authorRawAuthor =
  ( Author { authorId = 1, authorUserId = 1, authorDescription = "New here" }
  , User { userId          = 1
         , userName        = "Newauthor"
         , userSurname     = "Authorovich"
         , userAvatar      = "http"
         , userDateCreated = sometime
         , userIsAdmin     = False
         }
  )

newtype AuthorAndUserRaw = AuthorAndUserRaw (AuthorRaw, UserRaw)

instance ToJSON AuthorAndUserRaw where
  toJSON (AuthorAndUserRaw (AuthorRaw {..}, UserRaw {..})) = object
    [ "name" .= userRawName
    , "surname" .= userRawSurname
    , "avatar" .= userRawAvatar
    , "description" .= authorRawDescription
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
    it "responds with 400 with incorrect JSON structure"
      $                   post "/api/users/" "{mistake}"
      `shouldRespondWith` 400

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

  describe "GET /api/authors/" $ do
    it "responds with 404 without authorization header"
      $                   get "/api/authors/"
      `shouldRespondWith` 404
    it "responds with 404 with incorrect authorization header"
      $ request "GET" "/api/authors/" [("Authorization", "1")] ""
      `shouldRespondWith` 404
    it "responds with 200 with correct authorization header"
      $ request "GET" "/api/authors/" [("Authorization", "5")] ""
      `shouldRespondWith` 200
    it "responds with correct JSON on calling with default limit and offset"
      $ request "GET" "/api/authors/" [("Authorization", "5")] ""
      `shouldRespondWith` 200
                            { matchBody =
                              bodyEquals
                                (encode $ authorToResponse <$> take 3 authors)
                            }
    it "responds with correct JSON on calling with custom limit and offset"
      $                   request "GET"
                                  "/api/authors?limit=2&offset=1"
                                  [("Authorization", "5")]
                                  ""
      `shouldRespondWith` 200
                            { matchBody =
                              bodyEquals
                                (   encode
                                $   authorToResponse
                                <$> (take 2 $ drop 1 authors)
                                )
                            }

  describe "POST /api/authors/" $ do
    it "responds with 404 without authorization header"
      $                   post "/api/authors/" ""
      `shouldRespondWith` 404
    it "responds with 404 with incorrect authorization header"
      $ request "POST" "/api/authors/" [("Authorization", "1")] ""
      `shouldRespondWith` 404
    it "responds with 200 with correct authorization header"
      $                   request "POST"
                                  "/api/authors/"
                                  [("Authorization", "5")]
                                  (encode $ AuthorAndUserRaw authorRaw)
      `shouldRespondWith` 200
    it "responds with 400 with incorrect JSON structure"
      $ request "POST" "/api/authors/" [("Authorization", "5")] "{mistake}"
      `shouldRespondWith` 400
    it "responds with correct JSON"
      $                   request "POST"
                                  "/api/authors/"
                                  [("Authorization", "5")]
                                  (encode $ AuthorAndUserRaw authorRaw)
      `shouldRespondWith` 200
                            { matchBody =
                              bodyEquals
                                (encode $ authorToResponse authorRawAuthor)
                            }

  describe "DELETE /api/authors/" $ do
    it "responds with 404 without authorization header"
      $                   delete "/api/authors/1"
      `shouldRespondWith` 404
    it "responds with 404 with incorrect authorization header"
      $ request "DELETE" "/api/authors/1" [("Authorization", "1")] ""
      `shouldRespondWith` 404
    it "responds with 204 with correct authorization header"
      $ request "DELETE" "/api/authors/1" [("Authorization", "5")] ""
      `shouldRespondWith` 204
    it "responds with 422 with incorrect author id"
      $ request "DELETE" "/api/authors/66" [("Authorization", "5")] ""
      `shouldRespondWith` 422
