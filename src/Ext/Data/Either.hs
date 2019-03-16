module Ext.Data.Either where

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

listToEither :: e -> [a] -> Either e a
listToEither e [] = Left e
listToEither _ (x:_) = Right x
