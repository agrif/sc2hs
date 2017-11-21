{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.SC2.Requestable
  ( Requestable(..)
  , extractResponse
  , extractResponseErr
  ) where

import Data.Proxy
import Data.Text
import Data.Maybe
import qualified Proto.S2clientprotocol.Sc2api as A
import Data.Default.Class
import Lens.Family2

class Requestable a where
  type ResponseOf a
  toRequest :: a -> A.Request
  fromResponse :: Proxy a -> A.Response -> Either String (ResponseOf a)

instance Requestable A.Request where
  type ResponseOf A.Request = A.Response
  toRequest = id
  fromResponse _ = Right

extractResponse :: (A.Response -> Maybe a) -> A.Response -> Either String a
extractResponse f r = maybe (Left (unpack (intercalate "\n" (A._Response'error r)))) Right (f r)

extractResponseErr :: (A.Response -> Maybe a) -> (a -> Maybe b) -> (a -> Maybe Text) -> A.Response -> Either String b
extractResponseErr f v e r =
  do x <- extractResponse f r
     case e x of
       Nothing  -> maybe (Left "error parsing payload") Right (v x)
       Just err -> Left (unpack err)
