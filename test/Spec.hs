module Main
  ( main
  ) where

import qualified MonadDatabaseSpec as MD
import qualified RouteSpec as RS
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  rs <- testSpec "Router Tests" RS.spec
  md <- testSpec "Database Tests" MD.spec
  defaultMain (testGroup "main tests" [rs, md])
