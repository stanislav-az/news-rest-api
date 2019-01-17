{-# LANGUAGE OverloadedStrings #-}

module RouteSpec
    ( spec
    )
where

import           Test.Hspec
import           Router                         ( parsePath
                                                , Route(..)
                                                )

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
spec = describe "route" $ do
    it "should route to root url GET" $ do
        let res = parsePath rootRoute [] "GET"
        res `shouldBe` True
    it "should not route to root url with wrong request method" $ do
        let res = parsePath rootRoute [] "POST"
        res `shouldBe` False
    it "should not route to non-existing GET url" $ do
        let res = parsePath rootRoute ["api", "unknown", "123"] "GET"
        res `shouldBe` False
    it "should route to user assign page for user 55 with POST" $ do
        let
            res = parsePath userAssignRoute
                            ["api", "users", "55", "assign"]
                            "POST"
        res `shouldBe` True
    it "should route to user reject page for user 55 with POST" $ do
        let
            res = parsePath userRejectRoute
                            ["api", "users", "55", "reject"]
                            "POST"
        res `shouldBe` True
    it "should not route if url is longer than existing routes" $ do
        let res = parsePath userRejectRoute
                            ["api", "users", "55", "reject", "something"]
                            "POST"
        res `shouldBe` False
    it "should not route if url is shorter than existing routes" $ do
        let res = parsePath userRejectRoute ["api", "users"] "POST"
        res `shouldBe` False
    it "should not route if url is empty" $ do
        let res = parsePath userRejectRoute [] "POST"
        res `shouldBe` False
    it "should not route if has wrong sub-path" $ do
        let res =
                parsePath userRejectRoute ["api", "user", "55", "reject"] "POST"
        res `shouldBe` False
    it "should route to dynamic root route" $ do
        let res =
                parsePath dynamicRoute ["123"] "POST"
        res `shouldBe` True



