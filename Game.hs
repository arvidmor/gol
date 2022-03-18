module Game where

import Types
import Data.Matrix (matrix, Matrix, setElem, fromLists, toLists, safeGet)

emptyGame :: Int -> Int -> Game
emptyGame rows columns = Game {grid = matrix rows columns (\(r, c) -> Cell (r, c) False), paused = False}

insertBlinker :: Int -> Int -> Game -> Game
insertBlinker row column game = game {grid = 
      setElem (Cell (row+2, column) True) (row+2, column)
    $ setElem (Cell (row+1, column) True) (row+1, column)
    $ setElem (Cell (row, column) True) (row, column) (grid game)}

spawn :: Coord -> Game -> Game
spawn (r, c) game = game {grid = setElem (Cell (r, c) True) (r, c) (grid game)}

kill :: Coord -> Game -> Game
kill (r, c) game    = game {grid = setElem (Cell (r, c) False) (r, c) (grid game)}

nextGeneration :: Game -> Game
nextGeneration game = game {grid = fromLists $ map (map (`transformCell` game)) (toLists (grid game))}

numberOfNeighbors :: Coord -> Game -> Int
numberOfNeighbors (r, c) game = foldl (\acc p -> if p == Just True then acc+1 else acc) 0 (getNeighboringCells (r, c) game)

getNeighboringCells :: Coord -> Game -> [Maybe Bool]
getNeighboringCells (r, c) game = map boolFromCell [safeGet r (c + 1) (grid game),
                                                    safeGet (r+1) (c+1) (grid game),
                                                    safeGet (r+1) c (grid game),
                                                    safeGet (r+1) (c-1) (grid game),
                                                    safeGet r (c-1) (grid game),
                                                    safeGet (r-1) (c-1) (grid game),
                                                    safeGet (r-1) c (grid game),
                                                    safeGet (r-1) (c+1) (grid game)
                                                    ]

boolFromCell :: Maybe Cell -> Maybe Bool
boolFromCell (Just (Cell coord bool)) = Just bool 
boolFromCell Nothing = Nothing

transformCell :: Cell -> Game -> Cell
transformCell cell@(Cell coord _) game = 
    let i = numberOfNeighbors coord game in
    case cell of 
    (Cell coord True)   | i >=2 && i <= 3   -> cell
                        | otherwise         -> Cell coord False
    (Cell coord False)  | i == 3            -> Cell coord True
                        | otherwise         -> cell