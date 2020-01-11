import Data.List (minimumBy)
import Data.Function (on)

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n ls = (take n ls) : chunk n (drop n ls)

count :: Eq a => a -> [a] -> Int
count n x = length $ filter (== n) x

choose :: Char -> Char -> Char
choose '1' _ = '1'
choose '0' _ = '0'
choose '2' x =  x

translate :: String -> String
translate xs = [ case x of
                  '1' -> '█'
                  '0' -> ' ' | x <- xs ]

main :: IO ()
main = do
  layers <- (chunk 150) 
            . init
            <$> readFile "../inputs/8.in"

  let minLayer = minimumBy (compare `on` (count '0')) layers

  let part1 = count '1' minLayer * count '2' minLayer
  let part2 = (unlines
               . map translate
               . chunk 25 
               . (foldr (zipWith choose)
                  . take 150 
                  . repeat) '2') layers

  putStrLn $ "Part 1: " ++ show part1

  putStrLn "Part 2:" 
  putStrLn part2
