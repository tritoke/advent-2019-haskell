import Data.List.Split (splitOn)
import Data.Sequence hiding (drop)

op :: Int -> Seq Int -> Seq Int
op pos nums
  | h == 1  = op newPos $ update c (a + b) nums
  | h == 2  = op newPos $ update c (a * b) nums
  | h == 99 = nums
  | otherwise = fromList []
  where
    h      = index nums pos
    a      = index nums $ index nums $ pos + 1
    b      = index nums $ index nums $ pos + 2
    c      = index nums $ pos + 3
    newPos = pos + 4

execute :: Seq Int -> Int
execute prog = index (op 0 prog) 0

program :: Int -> Int -> [Int] -> Seq Int
program noun verb prog = fromList $ head prog : [noun, verb] ++ drop 3 prog

main :: IO ()
main = do
  contents <- readFile "../inputs/2.in"
  let numbers = map (read::String->Int) $ splitOn "," contents
  let part1   = execute $ program 12 2 numbers
  let part2   = head [ 100 * noun + verb | noun <- [0..99], 
                                           verb <- [0..99],
                                          (execute $ program noun verb numbers) == 19690720 ]

  putStr "Part 1: "
  print part1
  putStr "Part 2: "
  print part2
