module Authorization
  ( authorizeAndHandle
  )
where

import           Handlers
import           Data.Text
import           Network.Wai


authorizeAndHandle :: DynamicPathsMap -> Request -> Handler -> IO Response
authorizeAndHandle dpMap req handle = undefined


{-
updateAuthorRoute :: Route
updateAuthorRoute =
  PathRoute "api"
    $ DynamicRoute "by"
    $ PathRoute "author"
    $ DynamicRoute "whom"
    $ MethodRoute "PATCH"
-}
