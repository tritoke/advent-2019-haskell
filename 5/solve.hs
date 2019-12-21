import Debug.Trace (traceShowId)
import Data.Char (digitToInt)
import Data.List.Split (splitOn)


modes_from_int :: Int -> [Int]
modes_from_int x = map digitToInt str
  where
    remLast2 = init . init . show
    str = case (length . show) x of
            2 -> "000"
            3 -> "001"
            4 -> '0' : remLast2 x
            5 -> remLast2 x

op :: Int -> Seq Int -> Seq Int
op pos nums =
  case traceShowId opcode of
    1 -> op newPos $! update op3 (op1 + op2) nums
    2 -> op newPos $! update op3 (op1 * op2) nums
    3 -> op newPos $! update op2 op1 nums
    4 -> drop 1 $ (f op1) <| nums
    9 -> nums
    where
      f       = index $ traceShowId nums
      opcode  = (digitToInt . last . show . f) pos
      modes   = modes_from_int $ f pos

      op1     = traceShowId $ case (modes !! 0) of
                  1 -> f $ pos + 1
                  0 -> f . f $ pos + 1

      op2     = traceShowId $ case opcode of
                  3 -> (read::String->Int) . unsafePerformIO $ getLine
                  _ -> case (modes !! 0) of
                         1 -> f $ pos + 2
                         0 -> f . f $ pos + 2

      op3     = traceShowId $ case (modes !! 1) of
                  1 -> f $ pos + 2
                  0 -> f . f $ pos + 2

      newPos  =
        case opcode of
          1 -> pos + 4
          2 -> pos + 4
          3 -> pos + 2
          4 -> pos + 2

execute :: Seq Int -> Seq Int
execute prog = op 0 prog

main :: IO ()
main = do
  let prog  = fromList .
              zip [0..] .
              map (read @Int) . 
              splitOn "," <$> 
              readFile "test.in"
  readFile "test.in"
  print prog
  let part1 = execute prog
  print part1

--putStr "Part 1: "
--print part1
