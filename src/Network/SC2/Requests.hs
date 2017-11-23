{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Network.SC2.Requests
       ( module Network.SC2.Types
       , Ping(..)
       , PingResponse(..)
       , Fog(..)
       , Realtime(..)
       , Seed(..)
       , CreateGame(..)
       , JoinGame(..)
       , RestartGame(..)
       -- , StartReplay(..)
       , LeaveGame(..)
       , QuickSave(..)
       , QuickLoad(..)
       , QuitGame(..)
       , GameInfo(..)
       , Step(..)
       , AvailableMaps(..)
       ) where

import qualified Proto.S2clientprotocol.Common as C
import qualified Proto.S2clientprotocol.Sc2api as A
import qualified Proto.S2clientprotocol.Raw as R

import Lens.Family2
import Control.Monad
import Data.Default.Class
import qualified Data.Text as T
import Data.Void
import Network.SC2.Requestable
import Network.SC2.Types
import Network.SC2.Convert

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

data Fog = Fog | NoFog
         deriving (Show, Eq, Enum)

data Realtime = Stepped | Realtime
              deriving (Show, Eq, Enum)

data Seed = Seed Int | RandomSeed
          deriving (Show, Eq)

data CreateGame = CreateGame Map [Player Race]
                | CreateGameFull Map [Player Race] Fog Seed Realtime
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


-- FIXME as observer? ports? render / featurelayer interface?
data JoinGame = JoinGame Race [Interface Void]
              deriving (Show, Eq)

instance Requestable JoinGame where
  type ResponseOf JoinGame = PlayerID
  toRequest (JoinGame r ifaces) = def & A.joinGame .~ mod def
    where
      mod = racemod . foldr (.) id (fmap ifacemod ifaces)

      racemod :: A.RequestJoinGame -> A.RequestJoinGame
      racemod = A.race .~ convertRace r

      ifacemod :: Interface Void -> A.RequestJoinGame -> A.RequestJoinGame
      ifacemod Raw = A.options . A.raw .~ True
      ifacemod Score = A.options . A.score .~ True
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

data QuitGame = QuitGame
          deriving (Show, Eq)

instance Requestable QuitGame where
  type ResponseOf QuitGame = ()
  toRequest _ = def & A.quit .~ A.RequestQuit
  fromResponse _ = void . extractResponse A._Response'quit

data GameInfo = GameInfo
              deriving (Show, Eq)

data GameInfoResponse =
  GameInfoResponse
  { mapName :: String
  , modNames :: [String]
  , localMapPath :: String
  , playerInfo :: [(PlayerID, Player RaceResolved)]
  , startRaw :: Maybe StartRaw
  , interfaces :: [Interface ()]
  } deriving (Eq, Show)

data StartRaw =
  StartRaw
  { mapSize :: (Int, Int)
  , pathingGrid :: ImageData
  , terrainHeight :: ImageData
  , placementGrid :: ImageData
  , playableArea :: ((Int, Int), (Int, Int))
  , startLocations :: [(Float, Float)]
  } deriving (Eq, Show)

instance Requestable GameInfo where
  type ResponseOf GameInfo = GameInfoResponse
  toRequest _ = def & A.gameInfo .~ A.RequestGameInfo
  fromResponse _ = extractResponse (A._Response'gameInfo >=> convert)
    where
      convert gi = do
        mname <- T.unpack <$> A._ResponseGameInfo'mapName gi
        let mods = T.unpack <$> A._ResponseGameInfo'modNames gi
        localpath <- T.unpack <$> A._ResponseGameInfo'localMapPath gi
        players <- traverse convertPlayer (A._ResponseGameInfo'playerInfo gi)
        let raw = convertRaw =<< A._ResponseGameInfo'startRaw gi
        let ifaces = (snd . iface A.maybe'raw id Raw . iface A.maybe'score id Score . iface A.maybe'featureLayer (const True) (FeatureLayer ()) . iface A.maybe'render (const True) (Render ())) (gi, [])
        return (GameInfoResponse mname mods localpath players raw ifaces)
      iface :: Lens' A.InterfaceOptions (Maybe b) -> (b -> Bool) -> a -> (A.ResponseGameInfo, [a]) -> (A.ResponseGameInfo, [a])
      iface l t x (gi, xs) = case gi ^. A.options . l of
        Just b | t b -> (gi, x : xs)
        _            -> (gi, xs)
      convertPlayer p = do
        pid <- PlayerID . fromIntegral <$> A._PlayerInfo'playerId p
        typ <- A._PlayerInfo'type' p
        ourtyp <- case typ of
          A.Observer -> return Observer
          A.Participant -> do
            race <- convertRace p
            return (Participant race)
          A.Computer -> do
            race <- convertRace p
            diff <- A._PlayerInfo'difficulty p
            return (Computer race diff)
        return (pid, ourtyp)
      convertRace p = do
        req <- A._PlayerInfo'raceRequested p
        let act = A._PlayerInfo'raceActual p
        return (convertRaceBack req (flip convertRaceBack (undefined) <$> act))
      convertRaw r = do
        msize <- convertFrom =<< (R._StartRaw'mapSize r)
        pagrid <- convertFrom =<< (R._StartRaw'pathingGrid r)
        theight <- convertFrom =<< (R._StartRaw'terrainHeight r)
        plgrid <- convertFrom =<< (R._StartRaw'placementGrid r)
        parea <- convertFrom =<< (R._StartRaw'playableArea r)
        starts <- traverse convertFrom (R._StartRaw'startLocations r)
        return (StartRaw msize pagrid theight plgrid parea starts)

-- FIXME RequestObservation

-- FIXME RequestAction

data Step = Step
          | StepMany Word

instance Requestable Step where
  type ResponseOf Step = ()
  toRequest Step = toRequest (StepMany 1)
  toRequest (StepMany i) = def & A.step . A.count .~ fromIntegral i
  fromResponse _ = void . extractResponse A._Response'step

data AvailableMaps = AvailableMaps
                   deriving (Show, Eq)

instance Requestable AvailableMaps where
  type ResponseOf AvailableMaps = [Map]
  toRequest _ = def & A.availableMaps .~ A.RequestAvailableMaps
  fromResponse _ = extractResponse (fmap makeMaps . A._Response'availableMaps)
    where makeMaps :: A.ResponseAvailableMaps -> [Map]
          makeMaps m = (LocalMap . T.unpack <$> m ^. A.localMapPaths <*> pure Nothing)
                       ++ (BattlenetMap . T.unpack <$> m ^. A.battlenetMapNames)
