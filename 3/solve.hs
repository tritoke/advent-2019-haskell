import Data.List (elemIndex)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace  (traceShowId)
import Data.Sequence (Seq, takeWhileL, takeWhileR, (><), (<|))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq (fromList)
import qualified Data.Set as Set

coordsFromDirection :: Char -> Int -> (Int, Int) -> [(Int, Int)]
coordsFromDirection dir dist (x, y)
  | dir == 'L' = [ (x - i, y) | i <- [1..dist] ]
  | dir == 'D' = [ (x, y - i) | i <- [1..dist] ]
  | dir == 'U' = [ (x, y + i) | i <- [1..dist] ]
  | dir == 'R' = [ (x + i, y) | i <- [1..dist] ]


updateCoord :: (Int, Int) -> Char -> Int -> (Int, Int)
updateCoord (x, y) dir dist =
  case dir of
    'R' -> (x + dist, y)
    'L' -> (x - dist, y)
    'U' -> (x, y + dist)
    'D' -> (x, y - dist)

coordsFromPath :: [[Char]] -> (Int, Int) -> [(Int,Int)]
coordsFromPath paths coord
  | not $ null paths = (coordsFromDirection dir dist coord) ++ (coordsFromPath (tail paths) n_coords)
  | otherwise = []
  where
    cmd = head paths
    dir = head cmd
    dist = read $ tail cmd
    n_coords = updateCoord coord dir dist

manhattenDistance :: (Int, Int) -> Int
manhattenDistance (x, y) = (abs x) + (abs y)

removeDupe :: (Int, Int) -> Seq (Int, Int) -> Seq (Int, Int)
removeDupe coord path = (takeWhileL (/= coord) path) >< coord <| coord <| takeWhileR (/= coord) path

main :: IO ()
main = do
  contents <- readFile "../inputs/3.in"
  let paths = map (\x -> coordsFromPath x (0,0)) $ map (splitOn ",") $ lines contents
  let path_a = paths !! 0
  let path_b = paths !! 1
  let crossings = Set.toList $ Set.intersection (Set.fromList path_a) $ Set.fromList path_b
  let part1 = minimum $ map manhattenDistance crossings
  let part2 = (minimum $ [ length $ removeDupe x $ Seq.fromList $ path_a ++ reverse path_b | x <- crossings ])

  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
