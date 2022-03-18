import UI ( app )
import Graphics.Vty ( defaultConfig, mkVty)
import Types ( Game, Tick(Tick) )
import Brick.BChan ( newBChan, writeBChan )
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (forever, void)
import Brick.Main (customMain, defaultMain)
import Game (emptyGame, insertBlinker, insertGlider)

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 800000 -- decides how fast your game moves
  let buildVty = mkVty defaultConfig
  initVty <- buildVty
  g <- initGame
  --TODO: Why does the program crash if a hit anything other than q at the beginning?
  void $ customMain initVty buildVty (Just chan) app g

initGame :: IO Game
initGame = defaultMain app (insertBlinker 8 8 (insertGlider 2 2 (emptyGame 10 10)))