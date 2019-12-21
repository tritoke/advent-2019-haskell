import Data.Complex

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

type Position = (Double, Double)

type Direction = Double

type AsteroidField = [Position]

type BaseView = HashMap Direction AsteroidField

direction :: Position -> Position -> Direction
direction base asteroid = phase $ x :+ y
  where
    x = fst asteroid - fst base
    y = snd asteroid - snd base

baseView :: AsteroidField -> Position -> BaseView
baseView asteroids base = HashMap.fromListWith (++) [ (direction base asteroid, [asteroid]) |
                                                       asteroid <- asteroids,
                                                       asteroid /= base ]

canSee :: AsteroidField -> Position -> Int
canSee asteroids base = (length . HashMap.elems) $ baseView asteroids base

main :: IO ()
main = do
  field <- lines <$> readFile "../inputs/10.in"
  let asteroids = [ (fromIntegral x, fromIntegral y) |
                      x <- [0 .. (pred . length) field],
                      y <- [0 .. (pred . length . head) field],
                      field !! y !! x == '#']

  let (part1, base) = maximum $ [ (canSee asteroids base, base) | base <- asteroids ]
  print base
  print part1
