{-# LANGUAGE ScopedTypeVariables #-}
module Network.SC2.Process
       ( ExecOptions(..)
       , ConnectOptions(..)
       , Starcraft
       , runRemote
       , runLocal
       , sendRequest
       , readResponse
       ) where

import Network.Socket (withSocketsDo)
import System.Process
import System.Directory (withCurrentDirectory)
import qualified Network.WebSockets as WS
import Control.Concurrent
import Control.Exception
import qualified Proto.S2clientprotocol.Sc2api as A
import Data.ProtoLens
import Network.SC2.Mainable
import Network.SC2.Requestable
import Network.SC2.Requests

data ExecOptions = ExecOptions
    { executable :: String
    , workingDirectory :: Maybe String
    , connection :: ConnectOptions
    } deriving (Eq, Show)

data ConnectOptions =
  ConnectOptions
  { listenAddress :: String
  , listenPort :: Int
  } deriving (Eq, Show)

instance OptParseable ExecOptions where
  optParse = ExecOptions
             <$> strOption (long "executable" <> short 'e' <> metavar "PATH" <> help "path to the starcraft 2 executable")
             <*> optional (strOption (long "working-dir" <> short 'w' <> metavar "PATH" <> help "change to this directory before launching starcraft"))
             <*> optParse

instance OptParseable ConnectOptions where
  optParse = ConnectOptions
             <$> strOption (long "address" <> metavar "ADDR" <> help "address to use to talk to SC2" <> value "127.0.0.1")
             <*> option auto (long "port" <> metavar "PORT" <> help "port to use to talk to SC2" <> value 5000)

data Starcraft =
  Starcraft
  { processHandle :: Maybe ProcessHandle
  , processConn :: WS.Connection
  }

starcraftConnectIntern :: (WS.Connection -> IO ()) -> ConnectOptions -> IO ()
starcraftConnectIntern act opt = withSocketsDo $ tryConnect 0
  where
    tryConnect i | i < 60 = connectOnce `catch` \(x :: SomeException) -> (threadDelay 1000000 >> tryConnect (i + 1))
                 | otherwise = connectOnce
 
    connectOnce = WS.runClient (listenAddress opt) (listenPort opt) "/sc2api" $ \conn -> do
      WS.forkPingThread conn 30
      act conn

runRemote :: (Starcraft -> IO ()) -> ConnectOptions -> IO ()
runRemote act = starcraftConnectIntern (act . Starcraft Nothing)

runLocal :: (Starcraft -> IO ()) -> ExecOptions -> IO ()
runLocal act opt = maybe id withCurrentDirectory (workingDirectory opt) $ withCreateProcess procinfo handler
  where
    handler stdin stdout stderr ph = do
      starcraftConnectIntern (\conn ->
                                 let sc = Starcraft (Just ph) conn
                                 in act sc >> sendRequest sc (toRequest Quit))
        (connection opt)
      waitForProcess ph
      return ()

    procinfo = proc (executable opt) ["-listen", listenAddress $ connection opt, "-port", show (listenPort $ connection opt), "-displayMode", "0", "-windowwidth", "1024", "-windowheight", "768"]

sendRequest :: Starcraft -> A.Request -> IO ()
sendRequest sc = WS.sendBinaryData (processConn sc) . encodeMessage

readResponse :: Starcraft -> IO (Either String A.Response)
readResponse sc = decodeMessage <$> WS.receiveData (processConn sc)
