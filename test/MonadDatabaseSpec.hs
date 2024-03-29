{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module MonadDatabaseSpec
  ( spec
  ) where

import News.Config (loadConfig)
import qualified Control.Exception as E (bracket)
import Control.Monad.Except
import qualified Data.Aeson as JSON (ToJSON(..), encode, object)
import Data.Aeson ((.=))
import qualified Data.Map.Strict as M (fromAscList)
import News.Database.Connection (connect)
import News.Database.Models.Author (Author(..), AuthorRaw(..))
import News.Database.Models.Category (CategoryNested(..), CategoryRaw(..))
import News.Database.Models.Commentary (Commentary(..))
import News.Database.Models.Tag (Tag(..), TagRaw(..))
import News.Database.Models.User (User(..), UserRaw(..))
import qualified Database.PostgreSQL.Simple as PSQL (close)
import Ext.Data.Text (textify)
import MockMonad
import MockRoutes (mockRoutes)
import qualified Network.Wai as W
  ( Application(..)
  , Request(..)
  , Response(..)
  , strictRequestBody
  )
import News.Serializer.Author (authorToResponse)
import News.Serializer.Category (categoryNestedToResponse)
import News.Serializer.Tag (tagToResponse)
import News.Serializer.User (userToResponse)
import Test.Hspec (Spec(..), describe, it)
import Test.Hspec.Wai (delete, get, post, request, shouldRespondWith, with)
import Test.Hspec.Wai.Matcher (bodyEquals, matchBody)
import News.WebServer.Application (newsServer)
import News.WebServer.Error (manageHandlerError)
import News.WebServer.HandlerClass (MonadLogger(..))
import News.WebServer.HandlerMonad (DynamicPathsMap(..), serverErrorResponse)
import News.WebServer.UrlParser.Pagination (Limit(..))

mockApp :: IO W.Application
mockApp = pure $ newsServer mockRoutes runMockHandler

database :: MockDB
database =
  DB
    (mapify users)
    (mapify authors)
    (mapify tags)
    (mapify categories)
    (mapify comments)
  where
    mapify = M.fromAscList . zip [1 ..]

users :: [User]
users =
  [ User
      { userId = 1
      , userName = "User1"
      , userSurname = "Userovich1"
      , userAvatar = "http1"
      , userDateCreated = sometime
      , userIsAdmin = False
      }
  , User
      { userId = 2
      , userName = "User2"
      , userSurname = "Userovich2"
      , userAvatar = "http2"
      , userDateCreated = sometime
      , userIsAdmin = False
      }
  , User
      { userId = 3
      , userName = "User3"
      , userSurname = "Userovich3"
      , userAvatar = "http3"
      , userDateCreated = sometime
      , userIsAdmin = False
      }
  , User
      { userId = 4
      , userName = "User4"
      , userSurname = "Userovich4"
      , userAvatar = "http4"
      , userDateCreated = sometime
      , userIsAdmin = False
      }
  , User
      { userId = 5
      , userName = "Admin"
      , userSurname = "Adminovich"
      , userAvatar = "http5"
      , userDateCreated = sometime
      , userIsAdmin = True
      }
  ]

authors :: [(Author, User)]
authors =
  [ ( Author
        {authorId = 1, authorUserId = 1, authorDescription = "Delicate soul1"}
    , User
        { userId = 1
        , userName = "Author1"
        , userSurname = "Authorovich1"
        , userAvatar = "http1"
        , userDateCreated = sometime
        , userIsAdmin = False
        })
  , ( Author
        {authorId = 2, authorUserId = 2, authorDescription = "Delicate soul2"}
    , User
        { userId = 2
        , userName = "Author2"
        , userSurname = "Authorovich2"
        , userAvatar = "http2"
        , userDateCreated = sometime
        , userIsAdmin = False
        })
  , ( Author
        {authorId = 3, authorUserId = 3, authorDescription = "Delicate soul3"}
    , User
        { userId = 3
        , userName = "Author3"
        , userSurname = "Authorovich3"
        , userAvatar = "http3"
        , userDateCreated = sometime
        , userIsAdmin = False
        })
  , ( Author
        {authorId = 4, authorUserId = 4, authorDescription = "Delicate soul4"}
    , User
        { userId = 4
        , userName = "Author4"
        , userSurname = "Authorovich4"
        , userAvatar = "http4"
        , userDateCreated = sometime
        , userIsAdmin = False
        })
  ]

tags :: [Tag]
tags =
  [ Tag {tagId = 1, tagName = "tag1"}
  , Tag {tagId = 2, tagName = "tag2"}
  , Tag {tagId = 3, tagName = "tag3"}
  , Tag {tagId = 4, tagName = "tag4"}
  ]

categories :: [CategoryNested]
categories =
  [ Parent 1 "categoryNested1"
  , Parent 2 "categoryNested2"
  , Parent 3 "categoryNested3"
  , Parent 4 "categoryNested4"
  ]

comments :: [Commentary]
comments =
  [ Commentary
      { commentaryId = 1
      , commentaryContent = "I am fascinated"
      , commentaryNewsId = 1
      , commentaryUserId = 1
      }
  ]

userRaw :: UserRaw
userRaw = UserRaw "Newuser" "Userovich" "http"

userRawUser :: User
userRawUser =
  User
    { userId = 1
    , userName = "Newuser"
    , userSurname = "Userovich"
    , userAvatar = "http"
    , userDateCreated = sometime
    , userIsAdmin = False
    }

instance JSON.ToJSON UserRaw where
  toJSON UserRaw {..} =
    JSON.object
      [ "name" .= userRawName
      , "surname" .= userRawSurname
      , "avatar" .= userRawAvatar
      ]

authorRaw :: (AuthorRaw, UserRaw)
authorRaw =
  ( AuthorRaw {authorRawDescription = "New here"}
  , UserRaw
      { userRawName = "Newauthor"
      , userRawSurname = "Authorovich"
      , userRawAvatar = "http"
      })

authorRawAuthor :: (Author, User)
authorRawAuthor =
  ( Author {authorId = 1, authorUserId = 1, authorDescription = "New here"}
  , User
      { userId = 1
      , userName = "Newauthor"
      , userSurname = "Authorovich"
      , userAvatar = "http"
      , userDateCreated = sometime
      , userIsAdmin = False
      })

newtype AuthorAndUserRaw =
  AuthorAndUserRaw (AuthorRaw, UserRaw)

instance JSON.ToJSON AuthorAndUserRaw where
  toJSON (AuthorAndUserRaw (AuthorRaw {..}, UserRaw {..})) =
    JSON.object
      [ "name" .= userRawName
      , "surname" .= userRawSurname
      , "avatar" .= userRawAvatar
      , "description" .= authorRawDescription
      ]

tagRaw :: TagRaw
tagRaw = TagRaw "Newtag"

tagRawTag :: Tag
tagRawTag = Tag 1 "Newtag"

instance JSON.ToJSON TagRaw where
  toJSON TagRaw {..} = JSON.object ["name" .= tagRawName]

categoryRaw :: CategoryRaw
categoryRaw =
  CategoryRaw {categoryRawName = "cat", categoryRawParentId = Nothing}

categoryRawCategory :: CategoryNested
categoryRawCategory = Parent 1 "cat"

categoryRawWithParent :: CategoryRaw
categoryRawWithParent =
  CategoryRaw {categoryRawName = "cat", categoryRawParentId = Just 1}

categoryRawWithParentCategory :: CategoryNested
categoryRawWithParentCategory = CategoryNested 2 "cat" (head categories)

instance JSON.ToJSON CategoryRaw where
  toJSON (CategoryRaw name Nothing) = JSON.object ["name" .= name]
  toJSON (CategoryRaw name (Just id)) =
    JSON.object ["name" .= name, "parent_id" .= id]

runMockHandler :: W.Request -> DynamicPathsMap -> MockHandler -> IO W.Response
runMockHandler req dpMap handler = do
  conf <- loadConfig
  body <- W.strictRequestBody req
  res <-
    E.bracket (connect conf) PSQL.close $ \conn ->
      pure $
      runMock database body (Limit 3) dpMap req conn $
      catchError handler manageHandlerError
  either (\e -> (logError $ textify e) >> serverErrorResponse) pure res

spec :: Spec
spec =
  with mockApp $ do
    describe "GET /" $ it "responds with 200" $ get "/" `shouldRespondWith` 200
    describe "GET /api/users/" $ do
      it "responds with 200" $ get "/api/users/" `shouldRespondWith` 200
      it "responds with correct JSON on calling with default limit and offset" $
        get "/api/users/" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals (JSON.encode $ userToResponse <$> take 3 users)
          }
      it "responds with correct JSON on calling with custom limit and offset" $
        get "/api/users?limit=2&offset=1" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $ userToResponse <$> (take 2 $ drop 1 users))
          }
    describe "POST /api/users/" $ do
      it "responds with 200" $
        post "/api/users/" (JSON.encode userRaw) `shouldRespondWith` 200
      it "responds with correct JSON" $
        post "/api/users/" (JSON.encode userRaw) `shouldRespondWith`
        200 {matchBody = bodyEquals (JSON.encode $ userToResponse userRawUser)}
      it "responds with 400 with incorrect JSON structure" $
        post "/api/users/" "{mistake}" `shouldRespondWith` 400
    describe "DELETE /api/users/" $ do
      it "responds with 404 without authorization header" $
        delete "/api/users/1" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "DELETE" "/api/users/1" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 204 with correct authorization header" $
        request "DELETE" "/api/users/1" [("Authorization", "5")] "" `shouldRespondWith`
        204
      it "responds with 422 with incorrect user id" $
        request "DELETE" "/api/users/66" [("Authorization", "5")] "" `shouldRespondWith`
        422
    describe "GET /api/authors/" $ do
      it "responds with 404 without authorization header" $
        get "/api/authors/" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "GET" "/api/authors/" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 200 with correct authorization header" $
        request "GET" "/api/authors/" [("Authorization", "5")] "" `shouldRespondWith`
        200
      it "responds with correct JSON on calling with default limit and offset" $
        request "GET" "/api/authors/" [("Authorization", "5")] "" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals (JSON.encode $ authorToResponse <$> take 3 authors)
          }
      it "responds with correct JSON on calling with custom limit and offset" $
        request
          "GET"
          "/api/authors?limit=2&offset=1"
          [("Authorization", "5")]
          "" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $ authorToResponse <$> (take 2 $ drop 1 authors))
          }
    describe "POST /api/authors/" $ do
      it "responds with 404 without authorization header" $
        post "/api/authors/" "" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "POST" "/api/authors/" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 200 with correct authorization header" $
        request
          "POST"
          "/api/authors/"
          [("Authorization", "5")]
          (JSON.encode $ AuthorAndUserRaw authorRaw) `shouldRespondWith`
        200
      it "responds with 400 with incorrect JSON structure" $
        request "POST" "/api/authors/" [("Authorization", "5")] "{mistake}" `shouldRespondWith`
        400
      it "responds with correct JSON" $
        request
          "POST"
          "/api/authors/"
          [("Authorization", "5")]
          (JSON.encode $ AuthorAndUserRaw authorRaw) `shouldRespondWith`
        200
          { matchBody =
              bodyEquals (JSON.encode $ authorToResponse authorRawAuthor)
          }
    describe "DELETE /api/authors/" $ do
      it "responds with 404 without authorization header" $
        delete "/api/authors/1" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "DELETE" "/api/authors/1" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 204 with correct authorization header" $
        request "DELETE" "/api/authors/1" [("Authorization", "5")] "" `shouldRespondWith`
        204
      it "responds with 422 with incorrect author id" $
        request "DELETE" "/api/authors/66" [("Authorization", "5")] "" `shouldRespondWith`
        422
    describe "GET /api/tags/" $ do
      it "responds with 200" $ get "/api/tags/" `shouldRespondWith` 200
      it "responds with correct JSON on calling with default limit and offset" $
        get "/api/tags/" `shouldRespondWith`
        200
          {matchBody = bodyEquals (JSON.encode $ tagToResponse <$> take 3 tags)}
      it "responds with correct JSON on calling with custom limit and offset" $
        get "/api/tags?limit=2&offset=1" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $ tagToResponse <$> (take 2 $ drop 1 tags))
          }
    describe "POST /api/tags/" $ do
      it "responds with 404 without authorization header" $
        post "/api/tags/" "" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "POST" "/api/tags/" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 200 with correct authorization header" $
        request
          "POST"
          "/api/tags/"
          [("Authorization", "5")]
          (JSON.encode tagRaw) `shouldRespondWith`
        200
      it "responds with 400 with incorrect JSON structure" $
        request "POST" "/api/tags/" [("Authorization", "5")] "{mistake}" `shouldRespondWith`
        400
      it "responds with correct JSON" $
        request
          "POST"
          "/api/tags/"
          [("Authorization", "5")]
          (JSON.encode tagRaw) `shouldRespondWith`
        200 {matchBody = bodyEquals (JSON.encode $ tagToResponse tagRawTag)}
    describe "DELETE /api/tags/" $ do
      it "responds with 404 without authorization header" $
        delete "/api/tags/1" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "DELETE" "/api/tags/1" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 204 with correct authorization header" $
        request "DELETE" "/api/tags/1" [("Authorization", "5")] "" `shouldRespondWith`
        204
      it "responds with 422 with incorrect tag id" $
        request "DELETE" "/api/tags/66" [("Authorization", "5")] "" `shouldRespondWith`
        422
    describe "GET /api/categories/" $ do
      it "responds with 200" $ get "/api/categories/" `shouldRespondWith` 200
      it "responds with correct JSON on calling with default limit and offset" $
        get "/api/categories/" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $ categoryNestedToResponse <$> take 3 categories)
          }
      it "responds with correct JSON on calling with custom limit and offset" $
        get "/api/categories?limit=2&offset=1" `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $
                 categoryNestedToResponse <$> (take 2 $ drop 1 categories))
          }
    describe "POST /api/categories/" $ do
      it "responds with 404 without authorization header" $
        post "/api/categories/" "" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "POST" "/api/categories/" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 200 with correct authorization header" $
        request
          "POST"
          "/api/categories/"
          [("Authorization", "5")]
          (JSON.encode categoryRaw) `shouldRespondWith`
        200
      it "responds with 400 with incorrect JSON structure" $
        request "POST" "/api/categories/" [("Authorization", "5")] "{mistake}" `shouldRespondWith`
        400
      it "responds with correct JSON on calling without parent id" $
        request
          "POST"
          "/api/categories/"
          [("Authorization", "5")]
          (JSON.encode categoryRaw) `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $ categoryNestedToResponse categoryRawCategory)
          }
      it "responds with correct JSON on calling with parent id" $
        request
          "POST"
          "/api/categories/"
          [("Authorization", "5")]
          (JSON.encode categoryRawWithParent) `shouldRespondWith`
        200
          { matchBody =
              bodyEquals
                (JSON.encode $
                 categoryNestedToResponse categoryRawWithParentCategory)
          }
    describe "DELETE /api/categories/" $ do
      it "responds with 404 without authorization header" $
        delete "/api/categories/1" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "DELETE" "/api/categories/1" [("Authorization", "1")] "" `shouldRespondWith`
        404
      it "responds with 204 with correct authorization header" $
        request "DELETE" "/api/categories/1" [("Authorization", "5")] "" `shouldRespondWith`
        204
      it "responds with 422 with incorrect category id" $
        request "DELETE" "/api/categories/66" [("Authorization", "5")] "" `shouldRespondWith`
        422
    describe "DELETE /api/comments/" $ do
      it "responds with 404 without authorization header" $
        delete "/api/comments/1" `shouldRespondWith` 404
      it "responds with 404 with incorrect authorization header" $
        request "DELETE" "/api/comments/1" [("Authorization", "3")] "" `shouldRespondWith`
        404
      it "responds with 204 with correct authorization header" $
        request "DELETE" "/api/comments/1" [("Authorization", "1")] "" `shouldRespondWith`
        204
      it "responds with 204 with admin authorization header" $
        request "DELETE" "/api/comments/1" [("Authorization", "5")] "" `shouldRespondWith`
        204
      it "responds with 422 with incorrect comment id" $
        request "DELETE" "/api/comments/66" [("Authorization", "5")] "" `shouldRespondWith`
        422
