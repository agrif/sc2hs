{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Network.SC2.Split
       ( module Control.Monad.Freer
       , Split(..)
       , suspend
       , fork
       , halt
       , stepSplit
       , runSplit
       ) where

import Control.Monad.Freer
import Data.Maybe
import Data.Sequence

data Split a where
  Split :: Maybe a -> Maybe a -> Split a

suspend :: (Member Split r) => Eff r ()
suspend = send (Split Nothing (Just ()))

fork :: (Member Split r) => Eff r () -> Eff r ()
fork t = do
  side <- send (Split (Just False) (Just True))
  case side of
    False -> return ()
    True  -> t >> halt

halt :: (Member Split r) => Eff r a
halt = send (Split Nothing Nothing)

-- threads to run now, and threads to run later
data Status r = Status (Maybe (Eff r (Status r))) (Maybe (Eff r (Status r)))

stepSplit :: Eff (Split ': r) () -> Eff r (Status r)
stepSplit = handleRelay (\_ -> pure (Status Nothing Nothing)) go
  where
    go (Split now later) next = pure (Status (fmap next now) (fmap next later))

runSplit :: Eff (Split ': r) () -> Eff r ()
runSplit = go empty . stepSplit
  where
    go :: Seq (Eff r (Status r)) -> Eff r (Status r) -> Eff r ()
    go threads act = do
      Status now later <- act
      let newthreads = catMaybe now threads later
      case viewl newthreads of
        EmptyL  -> pure ()
        t :< ts -> go ts t

    catMaybe :: Maybe a -> Seq a -> Maybe a -> Seq a
    catMaybe mx t my = (maybe id (flip (|>)) my . maybe id (<|) mx) t
