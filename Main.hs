import UI 
import Graphics.Vty 
import Types 
import Brick.BChan 
import Control.Concurrent 
import Control.Monad 
import Brick.Main 
import Game 
import GHC.IO.Handle 
import GHC.IO.FD 

main :: IO ()
main = do
  rows <- getLineLoop isInteger "How many rows would you like in the grid?"
  cols <- getLineLoop isInteger "Columns?"
  initGame <- defaultMain editorApp (emptyGame (read rows) (read cols))
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 800000 -- decides how fast your game moves
  let buildVty = mkVty defaultConfig
  initVty <- buildVty
  void $ customMain initVty buildVty (Just chan) app initGame


--Provide a func such that if func answer then exit the loop, else try again.
getLineLoop :: (String -> Bool) -> String -> IO String
getLineLoop func prompt = do 
  putStrLn prompt
  answer <- getLine
  if func answer then return answer
  else
    putStrLn "Try again!" >>
    getLineLoop func prompt
