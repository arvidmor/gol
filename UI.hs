module UI where

import Types

import Data.Matrix
import Data.List.Split

import Brick hiding (Down, Up)
import Brick.Main
import Graphics.Vty
import Game (nextGeneration, step, toggleCell)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Table
import Prelude hiding (Right, Left)

app :: App Game Tick Name
app = App { appDraw         = drawGame
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEvent
          , appStartEvent   = return
          , appAttrMap      = const attributes
          }

editorApp :: App Game a Name
editorApp = App { 
            appDraw         = drawEditor
          , appChooseCursor = neverShowCursor
          , appHandleEvent  = handleEventEditor
          , appStartEvent   = return
          , appAttrMap      = const attributes
          }

aliveAttr, focusedAttr, focusedAliveAttr :: AttrName
aliveAttr           = attrName "Alive"
focusedAttr         = attrName "focused"
focusedAliveAttr    = attrName "focusedAlive"

attributes :: AttrMap
attributes = attrMap defAttr [
    (aliveAttr, bg white), 
    (focusedAttr, bg blue),
    (focusedAliveAttr, fg brightBlue)
    ]

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                           = continue $ nextGeneration g
handleEvent g (VtyEvent (EvKey (KChar 'q') []))         = halt g
handleEvent g _                                         = continue g

handleEventEditor :: Game -> BrickEvent Name a -> EventM Name (Next Game)
handleEventEditor g (VtyEvent (EvKey (KChar 'q') [])) = halt g
handleEventEditor g (VtyEvent (EvKey key [])) =
    continue newGame where 
    newGame =
        case key of 
            KUp             -> step Up g
            KDown           -> step Down g
            KLeft           -> step Left g
            KRight          -> step Right g
            KEnter          -> toggleCell (focused g) g
            (KChar 'w')     -> step Up g
            (KChar 's')     -> step Down g
            (KChar 'a')     -> step Left g
            (KChar 'd')     -> step Right g
            _               -> g
handleEventEditor g _   =  continue g

drawGame :: Game -> [Widget Name]
drawGame game = [drawGrid game]

drawGrid :: Game -> Widget Name
drawGrid game = renderTable 
    $ table
    $ chunksOf (snd (size game))
    $ map drawCell
    $ toList (grid game)

drawEditor :: Game -> [Widget Name]
drawEditor game = [drawGridEditor game]

drawGridEditor :: Game -> Widget Name
drawGridEditor game = renderTable 
    $ table
    $ chunksOf (snd (size game))
    $ map (drawCellCursor game)
    $ toList (grid game)



drawCell ::  Cell -> Widget Name
drawCell (Cell coord alive)  = 
    if alive then str "███" else str "   "

drawCellCursor :: Game -> Cell -> Widget Name
drawCellCursor game (Cell coord alive)      
    | coord == focused game && alive    = withAttr focusedAliveAttr $ str "███"
    | alive                             = withAttr aliveAttr $ str "███"
    | coord == focused game             = withAttr focusedAttr $ str "   "
    | otherwise                         = str "   "