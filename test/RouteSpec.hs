{-# LANGUAGE OverloadedStrings #-}

module RouteSpec
    ( spec
    )
where

import           Test.Hspec                     ( Spec(..)
                                                , shouldBe
                                                , describe
                                                , it
                                                )
import           Network.Wai                    ( Request(..)
                                                , responseLBS
                                                , defaultRequest
                                                , responseStatus
                                                )
import qualified Network.HTTP.Types            as HTTP
                                                ( status200
                                                , status404
                                                )
import           WebServer.Router               ( checkout
                                                , Route(..)
                                                , route
                                                )

rootRoute = MethodRoute "GET"

dynamicRoute = DynamicRoute "pk" $ MethodRoute "POST"

userAssignRoute :: Route
userAssignRoute =
    PathRoute "api"
        $ PathRoute "users"
        $ DynamicRoute "pk"
        $ PathRoute "assign"
        $ MethodRoute "POST"

userRejectRoute :: Route
userRejectRoute =
    PathRoute "api"
        $ PathRoute "users"
        $ DynamicRoute "pk"
        $ PathRoute "reject"
        $ MethodRoute "POST"

spec :: Spec
spec = do
    describe "checkout" $ do
        it "should route to root url GET" $ do
            let res = checkout [] rootRoute [] "GET"
            res `shouldBe` (True, [])
        it "should route to root url with / GET" $ do
            let res = checkout [] rootRoute [""] "GET"
            res `shouldBe` (True, [])
        it "should not route to root url with wrong request method" $ do
            let res = checkout [] rootRoute [] "POST"
            (fst res) `shouldBe` False
        it "should not route to non-existing GET url" $ do
            let res = checkout [] rootRoute ["api", "unknown", "123"] "GET"
            (fst res) `shouldBe` False
        it
                "should route to user assign page for user 55 with POST and get dynamic path"
            $ do
                  let
                      res = checkout []
                                     userAssignRoute
                                     ["api", "users", "55", "assign"]
                                     "POST"
                  res `shouldBe` (True, [("pk", "55")])
        it
                "should route to user reject page for user 55 with POST and get dynamic path"
            $ do
                  let
                      res = checkout []
                                     userRejectRoute
                                     ["api", "users", "55", "reject"]
                                     "POST"
                  res `shouldBe` (True, [("pk", "55")])
        it
                "should route to user assign page with / for user 55 with POST and get dynamic path"
            $ do
                  let
                      res = checkout []
                                     userAssignRoute
                                     ["api", "users", "55", "assign", ""]
                                     "POST"
                  res `shouldBe` (True, [("pk", "55")])
        it
                "should route to user reject page with / for user 55 with POST and get dynamic path"
            $ do
                  let
                      res = checkout []
                                     userRejectRoute
                                     ["api", "users", "55", "reject", ""]
                                     "POST"
                  res `shouldBe` (True, [("pk", "55")])
        it "should not route if url is longer than existing routes" $ do
            let res = checkout
                    []
                    userRejectRoute
                    ["api", "users", "55", "reject", "something"]
                    "POST"
            (fst res) `shouldBe` False
        it "should not route if url is shorter than existing routes" $ do
            let res = checkout [] userRejectRoute ["api", "users"] "POST"
            (fst res) `shouldBe` False
        it "should not route if url is empty" $ do
            let res = checkout [] userRejectRoute [] "POST"
            (fst res) `shouldBe` False
        it "should not route if has wrong sub-path" $ do
            let
                res = checkout []
                               userRejectRoute
                               ["api", "user", "55", "reject"]
                               "POST"
            (fst res) `shouldBe` False
        it "should route to dynamic root route and get dynamic path" $ do
            let res = checkout [] dynamicRoute ["123"] "POST"
            res `shouldBe` (True, [("pk", "123")])

    describe "route" $ do
        let responseOk =
                responseLBS HTTP.status200 [("Content-Type", "text/html")] "Ok"
            response404 =
                responseLBS HTTP.status404 [("Content-Type", "text/html")] ""
            createAuthorRoute =
                PathRoute "api" $ PathRoute "author" $ MethodRoute "POST"
            routes = [(createAuthorRoute, pure responseOk)]
            f _ _ h = h
        it "should not call handler on incorrect request" $ do
            res <- route routes defaultRequest f
            responseStatus res `shouldBe` HTTP.status404
        it "should call create author handler on POST /api/author" $ do
            let authorReq = defaultRequest { requestMethod = "POST"
                                           , pathInfo      = ["api", "author"]
                                           }
            res <- route routes authorReq f
            responseStatus res `shouldBe` HTTP.status200
        it "should not call create author handler on methods other than POST"
            $ do
                  let authorReq = defaultRequest { requestMethod = "GET"
                                                 , pathInfo = ["api", "author"]
                                                 }
                  res <- route routes authorReq f
                  responseStatus res `shouldBe` HTTP.status404
        it "should not call create author handler on incorrect path" $ do
            let authorReq = defaultRequest { requestMethod = "POST"
                                           , pathInfo      = ["api", "authors"]
                                           }
            res <- route routes authorReq f
            responseStatus res `shouldBe` HTTP.status404
        it "should not call any handlers and respond with 404 on empty routes"
            $ do
                  res <- route [] defaultRequest f
                  responseStatus res `shouldBe` HTTP.status404
        it "should call first handler on duplicate routes" $ do
            let routesDuplicate =
                    [ (createAuthorRoute, pure responseOk)
                    , ( createAuthorRoute
                      , error "Second route should not be called"
                      )
                    ]
                authorReq = defaultRequest { requestMethod = "POST"
                                           , pathInfo      = ["api", "author"]
                                           }
            res <- route routesDuplicate authorReq f
            responseStatus res `shouldBe` HTTP.status200

