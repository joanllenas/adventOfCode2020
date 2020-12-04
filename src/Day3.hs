module Day3 (day3, TerrainItem (..), JourneyLog (..), addSlope, getItemAtCoord) where

import Data.Bool
import qualified Utils

-- Model

type Coord = (Int, Int)

type Slope = (Int, Int)

data TerrainItem = Tree | OpenSquare deriving (Show, Eq)

type Grid = [[TerrainItem]]

data JourneyLog = MkJourneyLog
  { coord :: Coord,
    collisions :: Int,
    grid :: Grid
  }
  deriving (Show, Eq)

-- Utils

addSlope :: Slope -> Coord -> Coord
addSlope (x, y) (x', y') = (x + x', y + y')

getItemAtCoord :: Grid -> Coord -> Maybe TerrainItem
getItemAtCoord grid (x, y) =
  if y > length grid - 1
    then Nothing
    else
      let row = grid !! y
          cell = row !! mod x (length row)
       in Just cell

increaseCollisions :: TerrainItem -> Int -> Int
increaseCollisions item c = case item of
  Tree -> c + 1
  OpenSquare -> c

-- Main Loop

advanceUntilArrival :: Slope -> JourneyLog -> JourneyLog
advanceUntilArrival slope (MkJourneyLog coord c g) =
  let coord' = addSlope slope coord
      item = getItemAtCoord g coord'
   in case item of
        Nothing -> MkJourneyLog coord' c g
        Just item' -> advanceUntilArrival slope $ MkJourneyLog coord' (increaseCollisions item' c) g

-- Parsing

parseRow :: String -> [TerrainItem]
parseRow = map $ \ch -> bool OpenSquare Tree $ ch == '#'

parseInput :: String -> Grid
parseInput s =
  let rows = lines s
   in map parseRow rows

-- Part 1

part1 input =
  let g = parseInput input
      log = advanceUntilArrival (3, 1) $ MkJourneyLog (0, 0) 0 g
      collisions' = show . collisions $ log
   in "Part 1 result is: " ++ collisions' ++ " trees where encountered."

-- Part 2

part2 input =
  let g = parseInput input
      logs = map (\slope -> advanceUntilArrival slope $ MkJourneyLog (0, 0) 0 g) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
      collisions' = show $ foldr (\log n -> n * collisions log) 1 logs
   in "Part 2 result is: " ++ collisions' ++ " when multiplying together the number of trees encountered on each of the listed slopes."

-- Main

day3 :: IO ()
day3 = Utils.handleResultWith 3 part1 part2