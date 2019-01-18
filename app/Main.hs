{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Database
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp       ( run )
import           Data.List
import qualified Data.Text                     as T
import           Data.Aeson
import qualified Data.ByteString.Lazy          as LB
import qualified Data.ByteString.Lazy.Char8    as BC
import           Serializer

{-TODO
--Сделать data для Student
-}

app :: Application
app req respond = do
  let path = pathInfo req
  case path of
    [] -> do
      putStrLn "I've done some IO here"
      respond $ responseLBS
        status200
        [("Content-Type", "text/html")]
        "This is main page \n <a href=\"http://localhost:8080/nextpage\">Nextpage</a>"
    -- ["students"] -> do
    --   students <- getList "students" :: IO [(Int, T.Text, Int)]
    --   let jsonStudent (studentId, name, startYear) = object
    --         [ ("id"        , Number $ fromIntegral studentId)
    --         , ("name"      , String name)
    --         , ("start_year", Number $ fromIntegral startYear)
    --         ]
    --       printableStds = encode $ map jsonStudent students
    --   putStrLn "Students page accessed"
    --   respond $ responseLBS status200
    --                         [("Content-Type", "application/json")]
    --                         printableStds
    ["create-author"] -> case requestMethod req of
      "POST" -> do
        body <- requestBody req
        let createAuthorData =
              eitherDecode $ LB.fromStrict body :: Either
                  String
                  CreateAuthorRequest
        either reportParseError createAuthor createAuthorData
       where
        createAuthor authorData = do
          (user, author) <- addAuthorToDB $ requestToAuthor authorData
          let authorJSON = encode $ authorToResponse (user, author)
          putStrLn "Students page accessed"
          respond $ responseLBS status200
                                [("Content-Type", "application/json")]
                                authorJSON
        reportParseError err = respond $ responseLBS
          status400
          [("Content-Type", "plain/text")]
          ("Parse error: " <> BC.pack err)
      _ -> respond $ responseLBS status405
                                 [("Content-Type", "plain/text")]
                                 "Method not allowed"

    _ -> respond $ responseLBS status404
                               [("Content-Type", "plain/text")]
                               "Page not found"

logging :: Middleware
logging app req respond = app
  req
  (\res -> do
    let status = show $ statusCode $ responseStatus res
        method = show $ requestMethod req
        path   = show $ mconcat $ "/" : intersperse "/" (pathInfo req)
    putStrLn $ method ++ " " ++ path ++ " " ++ status
    respond res
  )

main :: IO ()
main = do
  putStrLn "Starting server at: \n"
  putStrLn $ "http://localhost:8080/"
  run 8080 (logging app)

