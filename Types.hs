module Types where
import Data.Matrix

--Each cell has a coord and bool to represent if they're alive or not
data Cell = Cell Coord Bool deriving Show

type Coord = (Int, Int)


type Grid = Matrix Cell 

data Game = Game {
    grid    :: Grid, 
    paused  :: Bool
} deriving Show

newtype Tick = Tick Int

type Name = ()

