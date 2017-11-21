module Network.SC2 (someFunc) where

import Network.SC2.Mainable
import Network.SC2.Process
import Network.SC2.Protocol
import Network.SC2.Requests

someFunc :: IO ()
someFunc = withMain mempty (runRemote (runSC2 bot))

bot :: SC2M ()
bot = do
  Right info <- syncRequest Ping
  send (putStrLn $ "info: " ++ show info)

  Right maps <- syncRequest AvailableMaps
  send (putStrLn $ "maps: " ++ show maps)

  r <- syncRequest $ CreateGame (LocalMap "/opt/StarCraftII/Maps/Simple64.SC2Map" Nothing) [Participant Protoss, Computer Terran Medium]
  send (putStrLn $ "create game: " ++ show r)

  r <- syncRequest $ JoinGame Protoss [Raw]
  send (putStrLn $ "join game: " ++ show r)
