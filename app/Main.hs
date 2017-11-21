module Main where

import Network.SC2

main :: IO ()
main = withMain mempty (runLocal (runSC2 bot))

bot :: SC2M ()
bot = do
  Right info <- syncRequest Ping
  send (putStrLn $ "info: " ++ show info)

  Right maps@(map : _) <- syncRequest AvailableMaps
  send (putStrLn $ "maps: " ++ show maps)

  r <- syncRequest $ CreateGame map [Participant Protoss, Computer Terran Medium]
  send (putStrLn $ "create game: " ++ show r)

  r <- syncRequest $ JoinGame Protoss [Raw]
  send (putStrLn $ "join game: " ++ show r)

