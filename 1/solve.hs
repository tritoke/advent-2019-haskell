{-# LANGUAGE TypeApplications #-}

mass_to_fuel :: Int -> Int
mass_to_fuel x = (div x 3) - 2

calc_fuel :: Int -> Int
calc_fuel fuel
  | new_fuel > 0 = new_fuel + calc_fuel new_fuel
  | otherwise = 0
  where
    new_fuel = mass_to_fuel fuel

main :: IO ()
main = do
  numbers <- map (read @Int)
             . lines
             <$> readFile "../inputs/1.in"
  let part1   = (sum . map mass_to_fuel) numbers
  let part2   = (sum . map calc_fuel) numbers
  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
