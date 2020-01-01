{-# LANGUAGE TupleSections #-}

-- original = http://ix.io/23qq/hs
-- credit = https://www.reddit.com/user/_oats/

import Control.Arrow
import Control.Monad.State
import Data.Foldable (toList)
import Data.List.Split (splitOn)
import Data.Sequence hiding (drop, take)

type Intcode = Seq Int
type Machine = State (Intcode, Int)

setMem :: Int -> Int -> Machine ()
setMem addr value = modify $ first $ update addr value

getMem :: Int -> Machine Int
getMem addr = gets $ (`index` addr) . fst

fetchOp :: Machine Int
fetchOp = gets $ uncurry (index)

advance :: Int -> Machine ()
advance x = modify $ second (+ x)

doMath :: (Int -> Int -> Int) -> Machine ()
doMath f = do
  (code, ip) <- get
  let (_ : src1 : src2 : dest : _) = drop ip $ toList code
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
  where [(noun, verb)] =
          [ (a, b) | a <- [0..99], b <- [0..99], result a b == 19690720 ]
        result a b = evalState (runWithParams a b) input

parse :: String -> Intcode
parse = fromList . map read . splitOn ","

main :: IO ()
main = do
  input <- (, 0) . parse <$> readFile "../../inputs/2.in"
  print (part1 input)
  print (part2 input)
