import Data.List.Split (splitOn)
import Data.List (group)
import Data.Char (digitToInt)

pred1 :: [Int] -> Bool
pred1 pass = all (\(x, y) -> x <= y) (zip pass $ tail pass)

pred2 :: [Int] -> Bool
pred2 pass = any (\(x, y) -> x == y) (zip pass $ tail pass)

pred3 :: [Int] -> Bool
pred3 pass = elem 2 $ map length $ group pass

main :: IO ()
main = do
  contents <- readFile "../inputs/4.in"
  let range = map (read::String->Int) $ splitOn "-" contents
  let passwords = [ [ digitToInt i | i <- show x ] | x <- [(range !! 0)..(range !! 1)] ]
  let rule1 = filter pred1 passwords
  let rule2 = filter pred2 rule1
  let rule3 = filter pred3 rule2

  let part1 = length rule2
  let part2 = length rule3

  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
