{-# LANGUAGE TypeFamilies #-}
module Network.SC2.Requests
       ( module Network.SC2.Types
       , Ping(..)
       , PingResponse(..)
       , Fog(..)
       , Realtime(..)
       , Seed(..)
       , CreateGame(..)
       , Interface(..)
       , PlayerID(..)
       , JoinGame(..)
       , RestartGame(..)
       -- , StartReplay(..)
       , LeaveGame(..)
       , QuickSave(..)
       , QuickLoad(..)
       , Quit(..)
       , AvailableMaps(..)
       ) where

import qualified Proto.S2clientprotocol.Common as C
import qualified Proto.S2clientprotocol.Sc2api as A

import Lens.Family2
import Control.Monad
import Data.Default.Class
import qualified Data.Text as T
import Network.SC2.Requestable
import Network.SC2.Types

data Ping = Ping
          deriving (Show, Eq)

data PingResponse =
  PingResponse
  { gameVersion :: String
  , dataVersion :: String
  , dataBuild :: Int
  , baseBuild :: Int
  } deriving (Show, Eq)

instance Requestable Ping where
  type ResponseOf Ping = PingResponse
  toRequest _ = def & A.ping .~ A.RequestPing
  fromResponse _ = extractResponse (A._Response'ping >=> convert)
    where convert (A.ResponsePing gv dv db bb) = PingResponse <$> (T.unpack <$> gv) <*> (T.unpack <$> dv) <*> (fromIntegral <$> db) <*> (fromIntegral <$> bb)

convertRace :: Race -> C.Race
convertRace Terran = C.Terran
convertRace Zerg = C.Zerg
convertRace Protoss = C.Protoss
convertRace Random = C.Random

convertPlayer :: Player -> A.PlayerSetup
convertPlayer Observer = A.PlayerSetup (Just A.Observer) Nothing Nothing
convertPlayer (Participant r) = A.PlayerSetup (Just A.Participant) (Just (convertRace r)) Nothing
convertPlayer (Computer r d) = A.PlayerSetup (Just A.Computer) (Just (convertRace r)) (Just d)

data Fog = Fog | NoFog
         deriving (Show, Eq, Enum)

data Realtime = Stepped | Realtime
              deriving (Show, Eq, Enum)

data Seed = Seed Int | RandomSeed
          deriving (Show, Eq)

data CreateGame = CreateGame Map [Player]
                | CreateGameFull Map [Player] Fog Seed Realtime
                deriving (Show, Eq)

instance Requestable CreateGame where
  type ResponseOf CreateGame = ()
  toRequest (CreateGame map players) = toRequest (CreateGameFull map players Fog RandomSeed Stepped)
  toRequest (CreateGameFull map players fog seed rt) = def & A.createGame .~ mods def
    where
      mods = mapmod map . playermod . fogmod . seedmod seed . rtmod

      fogmod :: A.RequestCreateGame -> A.RequestCreateGame
      fogmod = A.disableFog .~ (if fog == NoFog then True else False)

      seedmod :: Seed -> A.RequestCreateGame -> A.RequestCreateGame
      seedmod RandomSeed = id
      seedmod (Seed s) = A.randomSeed .~ fromIntegral s

      rtmod :: A.RequestCreateGame -> A.RequestCreateGame
      rtmod = A.realtime .~ (if rt == Realtime then True else False)

      mapmod :: Map -> A.RequestCreateGame -> A.RequestCreateGame
      mapmod (BattlenetMap m) = A.battlenetMapName .~ T.pack m
      mapmod (LocalMap m d) = A.localMap .~ A.LocalMap (Just (T.pack m)) d

      playermod :: A.RequestCreateGame -> A.RequestCreateGame
      playermod = A.playerSetup .~ fmap convertPlayer players
  fromResponse _ = void . extractResponseErr A._Response'createGame Just A._ResponseCreateGame'errorDetails


data Interface = Raw | Score | FeatureLayer | Render
               deriving (Show, Eq, Enum)

newtype PlayerID = PlayerID Int
                 deriving (Show, Eq)

-- FIXME as observer? ports
data JoinGame = JoinGame Race [Interface]
              deriving (Show, Eq)

instance Requestable JoinGame where
  type ResponseOf JoinGame = PlayerID
  toRequest (JoinGame r ifaces) = def & A.joinGame .~ mod def
    where
      mod = racemod . foldr (.) id (fmap ifacemod ifaces)

      racemod :: A.RequestJoinGame -> A.RequestJoinGame
      racemod = A.race .~ convertRace r

      ifacemod :: Interface -> A.RequestJoinGame -> A.RequestJoinGame
      ifacemod Raw = A.options . A.raw .~ True
      ifacemod Score = A.options . A.score .~ True
      ifacemod FeatureLayer = A.options . A.score .~ True
      ifacemod Render = A.options . A.score .~ True
  fromResponse _ = fmap (PlayerID . fromIntegral) . extractResponseErr A._Response'joinGame A._ResponseJoinGame'playerId A._ResponseJoinGame'errorDetails

data RestartGame = RestartGame
                 deriving (Show, Eq)


instance Requestable RestartGame where
  type ResponseOf RestartGame = ()
  toRequest _ = def & A.restartGame .~ A.RequestRestartGame
  fromResponse _ = void . extractResponseErr A._Response'restartGame Just A._ResponseRestartGame'errorDetails

-- StartReplay

data LeaveGame = LeaveGame
               deriving (Show, Eq)

instance Requestable LeaveGame where
  type ResponseOf LeaveGame = ()
  toRequest _ = def & A.leaveGame .~ A.RequestLeaveGame
  fromResponse _ = void . extractResponse A._Response'leaveGame

data QuickSave = QuickSave
               deriving (Show, Eq)

instance Requestable QuickSave where
  type ResponseOf QuickSave = ()
  toRequest _ = def & A.quickSave .~ A.RequestQuickSave
  fromResponse _ = void . extractResponse A._Response'quickSave

data QuickLoad = QuickLoad
               deriving (Show, Eq)

instance Requestable QuickLoad where
  type ResponseOf QuickLoad = ()
  toRequest _ = def & A.quickLoad .~ A.RequestQuickLoad
  fromResponse _ = void . extractResponse A._Response'quickLoad

data Quit = Quit
          deriving (Show, Eq)

instance Requestable Quit where
  type ResponseOf Quit = ()
  toRequest _ = def & A.quit .~ A.RequestQuit
  fromResponse _ = void . extractResponse A._Response'quit

data AvailableMaps = AvailableMaps
                   deriving (Show, Eq)

instance Requestable AvailableMaps where
  type ResponseOf AvailableMaps = [Map]
  toRequest _ = def & A.availableMaps .~ A.RequestAvailableMaps
  fromResponse _ = extractResponse (fmap makeMaps . A._Response'availableMaps)
    where makeMaps :: A.ResponseAvailableMaps -> [Map]
          makeMaps m = (LocalMap . T.unpack <$> m ^. A.localMapPaths <*> pure Nothing)
                       ++ (BattlenetMap . T.unpack <$> m ^. A.battlenetMapNames)
