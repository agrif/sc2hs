{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Network.SC2.Protocol
       ( module Control.Monad.Freer
       , module Network.SC2.Split
       , SC2Control(..)
       , SC2
       , SC2M
       , unsafeRequest
       , unsafeResponse
       , getStatus
       , request
       , syncRequest'
       , syncRequest
       , runSC2Control
       , runSC2
       ) where

import Control.Monad.Freer
import Control.Monad
import qualified Proto.S2clientprotocol.Sc2api as A
import Network.SC2.Requestable
import Network.SC2.Process
import Network.SC2.Split

data SC2Control a where
  SC2Request :: A.Request -> SC2Control()
  SC2Response :: SC2Control(Either String A.Response)
  SC2Status :: SC2Control A.Status

unsafeRequest :: (Member SC2Control r) => A.Request -> Eff r ()
unsafeRequest = send . SC2Request

unsafeResponse :: (Member SC2Control r) => Eff r (Either String A.Response)
unsafeResponse = send SC2Response

getStatus :: (Member SC2Control r) => Eff r A.Status
getStatus = send SC2Status

type SC2 r = (Member SC2Control r, Member Split r)

request :: (SC2 r, Requestable a) => a -> Eff r ()
request r = unsafeRequest (toRequest r) >> fork (void unsafeResponse)

syncRequest' :: (SC2 r, Requestable a) => a -> Eff r () -> Eff r (Either String (ResponseOf a))
syncRequest' r act = do
  unsafeRequest (toRequest r)
  act
  suspend
  resp <- unsafeResponse
  return (resp >>= fromResponse (pure r))

syncRequest :: (SC2 r, Requestable a) => a -> Eff r (Either String (ResponseOf a))
syncRequest r = syncRequest' r (pure ())

runSC2Control :: (Member IO r) => Eff (SC2Control ': r) a -> Starcraft -> Eff r a
runSC2Control m sc = runNatS A.Launched go m
  where
    go :: A.Status -> SC2Control a -> IO (A.Status, a)
    go s (SC2Request r) = (s,) <$> sendRequest sc r
    go s (SC2Response) = do
      resp <- readResponse sc
      case resp of
        Left _ -> return (s, resp)
        Right r -> case A._Response'status r of
          Nothing -> return (s, resp)
          Just s' -> return (s', resp)
    go s (SC2Status) = return (s, s)

type SC2M a = Eff '[SC2Control, Split, IO] a

runSC2:: SC2M () -> Starcraft -> IO ()
runSC2 m = runM . runSplit . runSC2Control m
