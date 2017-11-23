module Network.SC2.Types
  ( module Network.SC2.Types
  , A.Difficulty(..)
  , A.Status(..)
  ) where

import Data.Void
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Data.ByteString as BS

-- FIXME better image format
data ImageData = ImageData Int (Int, Int) BS.ByteString
  deriving (Show, Eq)

data Race' a = Terran
             | Zerg
             | Protoss
             | Random a
             deriving (Show, Eq)

type Race = Race' ()
type RaceResolved = Race' (Maybe (Race' Void))

data Interface c = Raw | Score | FeatureLayer c | Render c
                 deriving (Show, Eq)

newtype PlayerID = PlayerID Int
                 deriving (Show, Eq)

data Player r = Observer
              | Participant r
              | Computer r A.Difficulty
              deriving (Show, Eq)

data Map = BattlenetMap String
         | LocalMap String (Maybe BS.ByteString)
         deriving (Show, Eq)
