module Main
  ( main
  )
where

import           Test.Tasty                     ( defaultMain
                                                , testGroup
                                                )
import           Test.Tasty.Hspec               ( testSpec )
import qualified RouteSpec                     as RS
import qualified MonadDatabaseSpec             as MD

main :: IO ()
main = do
  rs <- testSpec "Router Tests" RS.spec
  md <- testSpec "Database Tests" MD.spec
  defaultMain (testGroup "main tests" [rs, md])
