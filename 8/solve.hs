chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n ls = (take n ls) : chunk n (drop n ls)

count :: Eq a => a -> [a] -> Int
count n x = length $ filter (\y -> y == n) x

choose :: (Char, Char) -> Char
choose ('1', _) = '1'
choose ('0', _) = '0'
choose ('2', x) =  x

update :: String -> String -> String
update a b = [ choose x | x <- zip a b]

translate :: String -> String
translate xs = [ case x of
                  '1' -> '█'
                  '0' -> ' ' | x <- xs ]

main :: IO ()
main = do
  layers <- (chunk 150) . init <$> readFile "../inputs/8.in"

  let (_, part1) = (minimum . map (\x -> (count '0' x, count '1' x * count '2' x))) layers
  let part2 = (unlines . map translate . chunk 25 . (foldr update . take 150 . repeat) '2') layers

  putStr "Part 1: "
  print part1

  putStrLn "Part 2:"
  putStrLn part2
