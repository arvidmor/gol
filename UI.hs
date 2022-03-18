module UI where

import Types

import Data.Matrix
import Data.List.Split

import Brick
import Brick.Main
import Graphics.Vty
import Game (nextGeneration)
import Brick.Widgets.Border

app :: App Game Tick Name
app = App { appDraw = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

aliveAttr = attrName "Alive"
theMap :: AttrMap
theMap = attrMap defAttr [(aliveAttr, bg white)]

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                           = continue $ nextGeneration g
handleEvent g (VtyEvent (EvKey (KChar 'q') []))         = halt g
handleEvent g _                                         = continue g

drawGame :: Game -> [Widget Name]
drawGame game = [drawGrid game]

drawGrid :: Game -> Widget Name
drawGrid game = vBox 
    $ map hBox
    $ chunksOf 10
    $ map drawCell
    $ toList (grid game)

drawCell :: Cell -> Widget Name
drawCell (Cell coord alive) = let box = joinBorders $ border (str "   ") in 
    if alive then withAttr aliveAttr box else box