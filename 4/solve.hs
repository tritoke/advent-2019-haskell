import Data.List.Split (splitOn)
import Data.List (group)
import Data.Char (digitToInt)


pred1 :: String -> Bool
pred1 [] = True
pred1 [s] = True
pred1 (s1:s2:sx) = s1 <= s2 && pred1 (s2:sx)

pred2 :: String -> Bool
pred2 = (2 <=) . maximum . map length . group

pred3 :: String -> Bool
pred3 = elem 2 . map length . group


main :: IO ()
main = do
  contents <- readFile "../inputs/4.in"
  let range = map (read::String->Int) $ splitOn "-" contents
      min = range !! 0
      max = range !! 1

      passwords = map show [(range !! 0)..(range !! 1)]

      pass1 = filter (\s -> pred1 s && pred2 s) passwords
      pass2 = filter pred3 pass1

      part1 = length pass1
      part2 = length pass2

  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
