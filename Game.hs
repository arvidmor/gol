module Game where

import Types
import Data.Char
import Data.Matrix (matrix, Matrix, setElem, fromLists, toLists, safeGet)
import Prelude hiding (Right, Left)

emptyGame :: Int -> Int -> Game
emptyGame rows columns = Game {grid = matrix rows columns (\(r, c) -> Cell (r, c) False), paused = False, size = (rows, columns), focused = (rows `div` 2, columns `div` 2)}

insertBlinker :: Int -> Int -> Game -> Game
insertBlinker row column game = game {grid =
      setElem (Cell (row+2, column) True) (row+2, column)
    $ setElem (Cell (row+1, column) True) (row+1, column)
    $ setElem (Cell (row, column) True) (row, column) (grid game)}

insertGlider :: Int -> Int -> Game -> Game
insertGlider row column game = game {grid =
      setElem (Cell (row, column+2) True) (row, column+2)
    $ setElem (Cell (row+1, column+2) True) (row+1, column+2)
    $ setElem (Cell (row+2, column+1) True) (row+2, column+1)
    $ setElem (Cell (row+2, column+2) True) (row+2, column+2)
    $ setElem (Cell (row+1, column) True) (row+1, column) (grid game)}

toggleCell :: Coord -> Game -> Game
toggleCell coord@(row, col) game = 
  if boolFromCell (safeGet row col (grid game)) == Just True then
    game {grid = setElem (Cell (row, col) False) (row, col) (grid game)}
  else 
    game {grid = setElem (Cell (row, col) True) (row, col) (grid game)}



nextGeneration :: Game -> Game
nextGeneration game = game {grid = fromLists $ map (map (`transformCell` game)) (toLists (grid game))}

numberOfNeighbors :: Coord -> Game -> Int
numberOfNeighbors (r, c) game = foldl (\acc p -> if p == Just True then acc+1 else acc) 0 (getNeighboringCells (r, c) game)

transformCell :: Cell -> Game -> Cell
transformCell cell@(Cell coord _) game =
    let i = numberOfNeighbors coord game in
    case cell of
    (Cell coord True)   | i >=2 && i <= 3   -> cell
                        | otherwise         -> Cell coord False
    (Cell coord False)  | i == 3            -> Cell coord True
                        | otherwise         -> cell

step :: Direction -> Game -> Game
step direction game = 
  let (maxR, maxC) = size game in
  (\(r, c) -> game {focused = (r, c)}) $ 
  case (direction, (r, c)) of
          (Up, (r, c))      | r == 1    -> (maxR, c)
                            | otherwise -> (r-1, c)

          (Down, (r, c))    | r == maxR -> (1, c)
                            | otherwise -> (r+1, c)

          (Left, (r, c))    | c == 1    -> (r, maxC)
                            | otherwise -> (r, c-1)

          (Right, (r, c))   | c == maxC -> (r, 1)
                            | otherwise -> (r, c+1)
          where
              (r, c) = focused game

--HELPER FUNCTIONS--

boolFromCell :: Maybe Cell -> Maybe Bool
boolFromCell (Just (Cell coord bool)) = Just bool
boolFromCell Nothing = Nothing

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

isInteger :: String -> Bool
isInteger "" = True
isInteger (char:str)  | not $ isDigit char  = False
                      | otherwise           = isInteger str

