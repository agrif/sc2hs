module Main where

import Network.SC2

main :: IO ()
main = withMain mempty (runLocal (runSC2 bot))

bot :: SC2M ()
bot = do
  Right info <- syncRequest Ping
  send (putStrLn $ "info: " ++ show info)

  Right maps@(map : _) <- syncRequest AvailableMaps
  --send (putStrLn $ "maps: " ++ show maps)

  Right () <- syncRequest $ CreateGameFull map [Participant Protoss, Computer (Random ()) Medium] Fog RandomSeed Realtime
  send (putStrLn "create game success")

  Right pid <- syncRequest $ JoinGame Protoss [Raw]
  send (putStrLn $ "join game: " ++ show pid)

  Right info <- syncRequest $ GameInfo
  --send (putStrLn $ "info: " ++ show info)

  loop

loop = do
  syncRequest Step
  status <- getStatus
  case status of
    Ended -> send (putStrLn "ending game")
    _     -> loop
