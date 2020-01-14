{-# LANGUAGE TypeApplications #-}

import           Data.Char       (digitToInt)
import           Data.List       (group)
import           Data.List.Split (splitOn)

pred1 :: String -> Bool
pred1 []         = True
pred1 [s]        = True
pred1 (s1:s2:sx) = s1 <= s2 && pred1 (s2:sx)

pred2 :: String -> Bool
pred2 = (2 <=) . maximum . map length . group

pred3 :: String -> Bool
pred3 = elem 2 . map length . group

main :: IO ()
main = do
  [start, end] <- map (read @Int)
                  . splitOn "-"
                  <$> readFile "../inputs/4.in"
  let passwords = map show [start..end]

      part1 = filter (\s -> pred1 s && pred2 s) passwords
      part2 = filter pred3 part1

  putStrLn $ "Part 1: " ++ (show . length) part1
  putStrLn $ "Part 2: " ++ (show . length) part2
