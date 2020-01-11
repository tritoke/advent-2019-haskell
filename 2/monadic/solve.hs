{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- original = http://ix.io/23qq/hs
-- credit = https://www.reddit.com/user/_oats/

import Control.Arrow (first, second)
import Control.Monad.State.Strict (State,
                                   gets,
                                   get,
                                   modify,
                                   evalState)
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap

type Intcode = IntMap Int
type Machine = State (Intcode, Int)

setMem :: Int -> Int -> Machine ()
setMem addr value = modify $ first $ IntMap.insert addr value

getMem :: Int -> Machine Int
getMem addr = gets $ (! addr) . fst

fetchOp :: Machine Int
fetchOp = gets $ uncurry (!)

advance :: Int -> Machine ()
advance x = modify $ second (+ x)

doMath :: (Int -> Int -> Int) -> Machine ()
doMath f = do
  (code, ip) <- get
  let src1 = code ! (ip + 1)
  let src2 = code ! (ip + 2)
  let dest = code ! (ip + 3)
  value <- f <$> getMem src1 <*> (getMem src2)
  setMem dest value
  advance 4

run :: Machine Int
run = do
  opcode <- fetchOp
  case opcode of
    1 -> doMath (+) >> run
    2 -> doMath (*) >> run
    99 -> getMem 0

runWithParams :: Int -> Int -> Machine Int
runWithParams a b = setMem 1 a >> setMem 2 b >> run

part1 :: (Intcode, Int) -> Int
part1 = evalState $ runWithParams 12 2

part2 :: (Intcode, Int) -> Int
part2 input = 100 * noun + verb
  where [(noun, verb)] = [ (a, b) | 
                            a <- [0..99],
                            b <- [0..99],
                            result a b == 19690720 ]
        result a b = evalState (runWithParams a b) input

main :: IO ()
main = do
  input <- (, 0)
           . IntMap.fromDistinctAscList
           . zip [0..]
           . map (read @Int)
           . splitOn ","
           <$> readFile "../../inputs/2.in"

  print $ part1 input
--print $ part2 input
