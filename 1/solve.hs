{-# LANGUAGE TypeApplications #-}

massToFuel :: Int -> Int
massToFuel x = (div x 3) - 2

calcFuel :: Int -> Int
calcFuel fuel
  | new_fuel > 0 = new_fuel + calcFuel new_fuel
  | otherwise = 0
  where
    new_fuel = massToFuel fuel

main :: IO ()
main = do
  numbers <- map (read @Int)
             . lines
             <$> readFile "../inputs/1.in"
  let part1   = (sum . map massToFuel) numbers
  let part2   = (sum . map calcFuel) numbers
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
