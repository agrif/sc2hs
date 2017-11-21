module Network.SC2.Types
  ( module Network.SC2.Types
  , A.Difficulty(..)
  ) where

import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Data.ByteString as BS

data Race = Terran
          | Zerg
          | Protoss
          | Random
          deriving (Show, Eq, Enum)

data Player = Observer
            | Participant Race
            | Computer Race A.Difficulty
            deriving (Show, Eq)

data Map = BattlenetMap String
         | LocalMap String (Maybe BS.ByteString)
         deriving (Show, Eq)
