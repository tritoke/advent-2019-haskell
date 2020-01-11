import Data.List.Split (splitOn)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HashMap

indirect :: HashMap String [String] -> String -> Int -> Int
indirect orbitmap key depth
  | depth == -1                 = (sum . map (\x -> indirect orbitmap x (succ depth))) next_keys
  | HashMap.member key orbitmap = depth + (sum . map (\x -> indirect orbitmap x (succ depth))) next_keys
  | otherwise                   = depth
  where next_keys = orbitmap ! key

path_from :: HashMap String [String] -> String -> String -> [String]
path_from orbitmap start end
  | not (HashMap.member start orbitmap) = []
  | end `elem` next_keys             = [ start, end ]
  | otherwise                        =
    case path of
      [] -> []
      _ -> start : path
  where
    next_keys = orbitmap ! start
    path = (concat . map (\x -> path_from orbitmap x end)) next_keys

collapse :: [String] -> [String]
collapse x
  | head x == last x = (collapse . tail . init) x
  | otherwise        = x

main :: IO ()
main = do
  orbits <- map (splitOn ")")
            . lines
            <$> readFile "../inputs/6.in"

  let orbit_map = HashMap.fromListWith (++) [ (k, [v]) | [k, v] <- orbits ]

  let part1 = length orbits + indirect orbit_map "COM" (-1)

  let part2 = (pred
               . pred
               . length
               . collapse
               . (\[x,y] -> x ++ reverse y)
               . map (path_from orbit_map "COM"))
               ["YOU", "SAN"]

  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
