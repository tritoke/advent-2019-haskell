{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

import Control.Arrow                (first, second)
import Control.Monad.RWS.Strict     (RWS, execRWS)
import Control.Monad.Reader         (asks)
import Control.Monad.Writer.Strict  (tell)
import Control.Monad.State.Strict   (gets, get, modify)
import Data.List                    (permutations)
import Data.List.Split              (splitOn)
import Data.IntMap.Strict           (IntMap, (!))
import qualified Data.IntMap.Strict as IntMap

type Intcode = IntMap Int
type Machine = RWS [Int] [Int] (Intcode, Int)

main :: IO ()
main = do
  input <- (, 0)
           . IntMap.fromDistinctAscList
           . zip [-1,0..]
           . (0 :)
           . map (read @Int)
           . splitOn ","
           <$> readFile "../inputs/7.in"

  putStr "part 1: "
  print $ part1 input

--putStr "part1: "
--print $ part1 input
--putStr "part2: "
--print $ part2 input

part1 :: (Intcode, Int) -> Int
part1 input = maximum $ map (thruster input 0) $ permutations [0..4]

-- use the returned state from execRWS to stop and restore machines
--part2 :: (Intcode, Int) -> Int
--part2 = last . snd . execRWS run [5]

thruster :: (Intcode, Int) -> Int -> [Int] -> Int
thruster code initial settings = foldl (amplifier code) initial settings

amplifier :: (Intcode, Int) -> Int -> Int -> Int
amplifier code input phase = (last . snd . execRWS run [phase, input]) code

run :: Machine ()
run = do
  (modes, opcode) <- ((flip divMod) 100) <$> fetchOp
  case opcode of
    1 -> doMath modes (+) >> run
    2 -> doMath modes (*) >> run
    3 -> doInput >> run
    4 -> doOutput modes >> run
    5 -> doJump modes (/= 0) >> run
    6 -> doJump modes (== 0) >> run
    7 -> doMath modes (cmp (<))  >> run
    8 -> doMath modes (cmp (==)) >> run
    99 -> return ()
    _ -> error $ "Unknown opcode: " ++ show opcode

setMem :: Int -> Int -> Machine ()
setMem addr value = modify $ first $ IntMap.insert addr value

getMem :: Int -> Machine Int
getMem addr = gets $ (! addr) . fst

fetchOp :: Machine Int
fetchOp = gets $ uncurry (!)

advance :: Int -> Machine ()
advance x = modify $ second (+ x)

jump :: Int -> Machine ()
jump ip = modify $ second (const ip)

getDigit :: Int -> Int -> Int
getDigit num digit = (num `div` 10 ^ digit) `mod` 10

parseParameter :: Int -> Int -> Machine Int
parseParameter modes position = do
  (code, ip) <- get
  let p = code ! (ip + position + 1)
  case getDigit modes position of
    0 -> return $ code ! p
    1 -> return p
    digit -> error $ "Invalid mode digit: " ++ show digit

rawParameter :: Int -> Machine Int
rawParameter position = gets (\(code, ip) -> code ! (ip + position + 1))
  
cmp :: (Int -> Int -> Bool) -> Int -> Int -> Int
cmp op a b
  | op a b    = 1
  | otherwise = 0

doMath :: Int -> (Int -> Int -> Int) -> Machine ()
doMath modes f = do
  (code, ip) <- get
  src1 <- parseParameter modes 0
  src2 <- parseParameter modes 1
  dest <- rawParameter 2
  let value = f src1 src2
  setMem dest value
  advance 4

doInput :: Machine ()
doInput = do
  (code, ip) <- get
  addr <- rawParameter 0
  let inputIndex = code ! (-1)
  value <- asks (!! inputIndex)
  setMem addr value
  setMem (-1) (inputIndex + 1)
  advance 2

doOutput :: Int -> Machine ()
doOutput modes = do
  value <- parseParameter modes 0
  tell [value]
  advance 2

doJump :: Int -> (Int -> Bool) -> Machine ()
doJump modes cond = do
  (code, ip) <- get
  src <- parseParameter modes 0
  dest <- parseParameter modes 1
  if cond src
    then jump dest
  else advance 3
