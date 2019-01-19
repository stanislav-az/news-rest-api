{-# LANGUAGE OverloadedStrings #-}

module RouteSpec
    ( spec
    )
where

import           Test.Hspec
import           Router                         ( isCorrectRoute
                                                , Route(..)
                                                , route
                                                )
import           Network.Wai
import           Network.HTTP.Types
import qualified Data.ByteString.Lazy          as L
import           Data.ByteString.Builder        ( Builder
                                                , toLazyByteString
                                                , word8
                                                )
import           Data.IORef

rootRoute = MethodRoute "GET"

dynamicRoute = DynamicRoute "pk" $ MethodRoute "POST"

userAssignRoute :: Route
userAssignRoute =
    PathRoute "api"
        $ PathRoute "users"
        $ DynamicRoute "55"
        $ PathRoute "assign"
        $ MethodRoute "POST"

userRejectRoute :: Route
userRejectRoute =
    PathRoute "api"
        $ PathRoute "users"
        $ DynamicRoute "55"
        $ PathRoute "reject"
        $ MethodRoute "POST"

spec :: Spec
spec = do
    describe "isCorrectRoute" $ do
        it "should route to root url GET" $ do
            let res = isCorrectRoute rootRoute [] "GET"
            res `shouldBe` True
        it "should not route to root url with wrong request method" $ do
            let res = isCorrectRoute rootRoute [] "POST"
            res `shouldBe` False
        it "should not route to non-existing GET url" $ do
            let res = isCorrectRoute rootRoute ["api", "unknown", "123"] "GET"
            res `shouldBe` False
        it "should route to user assign page for user 55 with POST" $ do
            let
                res = isCorrectRoute userAssignRoute
                                     ["api", "users", "55", "assign"]
                                     "POST"
            res `shouldBe` True
        it "should route to user reject page for user 55 with POST" $ do
            let
                res = isCorrectRoute userRejectRoute
                                     ["api", "users", "55", "reject"]
                                     "POST"
            res `shouldBe` True
        it "should not route if url is longer than existing routes" $ do
            let res = isCorrectRoute
                    userRejectRoute
                    ["api", "users", "55", "reject", "something"]
                    "POST"
            res `shouldBe` False
        it "should not route if url is shorter than existing routes" $ do
            let res = isCorrectRoute userRejectRoute ["api", "users"] "POST"
            res `shouldBe` False
        it "should not route if url is empty" $ do
            let res = isCorrectRoute userRejectRoute [] "POST"
            res `shouldBe` False
        it "should not route if has wrong sub-path" $ do
            let
                res = isCorrectRoute userRejectRoute
                                     ["api", "user", "55", "reject"]
                                     "POST"
            res `shouldBe` False
        it "should route to dynamic root route" $ do
            let res = isCorrectRoute dynamicRoute ["123"] "POST"
            res `shouldBe` True

    describe "route" $ do
        let responseOk =
                responseLBS status200 [("Content-Type", "text/html")] "Ok"
            response400 = responseLBS status400
                                      [("Content-Type", "text/html")]
                                      "Not valid"
            response404 = responseLBS status404
                                      [("Content-Type", "text/html")]
                                      "Not found"
            response500 = responseLBS status500
                                      [("Content-Type", "text/html")]
                                      "Sorry"
            createAuthorRoute =
                PathRoute "api" $ PathRoute "author" $ MethodRoute "POST"

            routes = [(createAuthorRoute, const $ pure responseOk)]
            getBody res = do
                let (_, _, f) = responseToStream res
                f $ \streamingBody -> do
                    builderRef <- newIORef mempty
                    let add :: Builder -> IO ()
                        add b = atomicModifyIORef builderRef
                            $ \builder -> (builder `mappend` b, ())
                        flush :: IO ()
                        flush = return ()
                    streamingBody add flush
                    fmap (L.toStrict . toLazyByteString) $ readIORef builderRef
        it "should not call handler on incorrect request" $ do
            res <- route routes defaultRequest
            getBody res `shouldReturn` "Not found"
            responseStatus res `shouldBe` status404
        it "should call create author handler on POST /api/author" $ do
            let authorReq = defaultRequest { requestMethod = "POST"
                                           , pathInfo      = ["api", "author"]
                                           }
            res <- route routes authorReq
            getBody res `shouldReturn` "Ok"
            responseStatus res `shouldBe` status200
        it "should not call create author handler on methods other than POST"
            $ do
                  let authorReq = defaultRequest { requestMethod = "GET"
                                                 , pathInfo = ["api", "author"]
                                                 }
                  res <- route routes authorReq
                  getBody res `shouldReturn` "Not found"
                  responseStatus res `shouldBe` status404
        it "should not call create author handler on incorrect path" $ do
            let authorReq = defaultRequest { requestMethod = "POST"
                                           , pathInfo      = ["api", "authors"]
                                           }
            res <- route routes authorReq
            getBody res `shouldReturn` "Not found"
            responseStatus res `shouldBe` status404
        it "should not call any handlers and respond with 404 on empty routes"
            $ do
                  res <- route [] defaultRequest
                  getBody res `shouldReturn` "Not found"
                  responseStatus res `shouldBe` status404
        it "should call first handler on duplicate routes" $ do
            let routesDuplicate =
                    [ (createAuthorRoute, const $ pure responseOk)
                    , ( createAuthorRoute
                      , const $ error "Second route should not be called"
                      )
                    ]
                authorReq = defaultRequest { requestMethod = "POST"
                                           , pathInfo      = ["api", "author"]
                                           }
            res <- route routesDuplicate authorReq
            getBody res `shouldReturn` "Ok"
            responseStatus res `shouldBe` status200

