{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Arrow               (first, second)
import           Control.Monad.Reader        (ask)
import           Control.Monad.RWS.Strict    (RWS, execRWS)
import           Control.Monad.State.Strict  (get, gets, modify)
import           Control.Monad.Writer.Strict (tell)
import           Data.IntMap.Strict          (IntMap, (!))
import qualified Data.IntMap.Strict          as IntMap
import           Data.List.Split             (splitOn)

type Intcode = IntMap Int
type Machine = RWS Int [Int] (Intcode, Int)

main :: IO ()
main = do
  input <- (, 0)
           . IntMap.fromDistinctAscList
           . zip [0..]
           . map (read @Int)
           . splitOn ","
           <$> readFile "../inputs/5.in"

  putStr "part1: "
  print $ part1 input
  putStr "part2: "
  print $ part2 input

part1 :: (Intcode, Int) -> Int
part1 = last . snd . execRWS run 1

part2 :: (Intcode, Int) -> Int
part2 = last . snd . execRWS run 5

run :: Machine ()
run = do
  (modes, opcode) <- flip divMod 100 <$> fetchOp
  case opcode of
    1  -> doMath modes (+) >> run
    2  -> doMath modes (*) >> run
    3  -> doInput >> run
    4  -> doOutput modes >> run
    5  -> doJump modes (/= 0) >> run
    6  -> doJump modes (== 0) >> run
    7  -> doMath modes (cmp (<))  >> run
    8  -> doMath modes (cmp (==)) >> run
    99 -> return ()
    _  -> error $ "Unknown opcode: " ++ show opcode

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
  p <- rawParameter position
  case getDigit modes position of
    0     -> return $ code ! p
    1     -> return p
    digit -> error $ "Invalid mode digit: " ++ show digit

rawParameter :: Int -> Machine Int
rawParameter position = gets $ uncurry (!) . second (+ succ position)

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
  value <- ask
  setMem addr value
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
