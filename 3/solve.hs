import Debug.Trace (traceShowId)
import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Sequence (Seq, takeWhileL, takeWhileR, (><), (<|))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (fromList)
import qualified Data.Set as Set

type Coord = (Int, Int)

coordsFromDirection :: Char -> Int -> Coord -> [Coord]
coordsFromDirection dir dist (x, y) =
  case dir of
    'L' -> [ (n_x, y) | n_x <- [x .. x - dist] ]
    'D' -> [ (x, n_y) | n_y <- [y .. y - dist] ]
    'U' -> [ (x, n_y) | n_y <- [y .. y + dist] ]
    'R' -> [ (n_x, y) | n_x <- [x .. x + dist] ]

updateCoord :: Coord -> Char -> Int -> Coord
updateCoord (x, y) dir dist =
  case dir of
    'R' -> (x + dist, y)
    'L' -> (x - dist, y)
    'U' -> (x, y + dist)
    'D' -> (x, y - dist)

coordsFromPath :: [[Char]] -> Coord -> [Coord]
coordsFromPath paths coord
  | not $ null paths = (coordsFromDirection dir dist coord) ++ (coordsFromPath (tail paths) n_coords)
  | otherwise = []
  where
    cmd = head paths
    dir = head cmd
    dist = read $ tail cmd
    n_coords = updateCoord coord dir dist

manhattenDistance :: Coord -> Int
manhattenDistance (x, y) = (abs x) + (abs y)

leftDistance :: [Coord] -> Coord -> Int
leftDistance list elem = (length . takeWhile (/= elem)) list

shortCircuitDistance :: [Coord] -> [Coord] -> Coord -> Int
shortCircuitDistance a b coord = 2 + leftDistance a coord + leftDistance b coord

main :: IO ()
main = do
  contents <- readFile "../inputs/3.in"
  let paths = map (\x -> coordsFromPath x (0,0)) $ map (splitOn ",") $ lines contents
  let path_a = paths !! 0
  let path_b = paths !! 1
  let crossings = Set.intersection (Set.fromList path_a) (Set.fromList path_b)
  let part1 = (Set.findMin . Set.map manhattenDistance) crossings
  putStrLn $ "Part 1: " ++ show part1
  let part2 = (Set.findMin . Set.map (shortCircuitDistance path_a path_b)) crossings
  putStrLn $ "Part 2: " ++ show part2
