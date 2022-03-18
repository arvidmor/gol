module UI where

import Types

import Data.Matrix

import Brick
import Brick.Main
import Graphics.Vty

app :: App Game Tick Name
app = App { appDraw = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

aliveAttr = attrName "Alive"
theMap :: AttrMap
theMap = attrMap defAttr [(aliveAttr, bg brightBlack)]

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent (Tick _))                       = continue g
handleEvent g _                                         = continue g

drawGame :: Game -> [Widget Name]
drawGame game = [drawGrid game]

drawGrid :: Game -> Widget Name
drawGrid game = vBox
    $ map drawCell
    $ toList (grid game)

drawCell :: Cell -> Widget Name
drawCell (Cell coord alive) | alive     = withAttr aliveAttr $ str "  "
                            | otherwise = str "  "