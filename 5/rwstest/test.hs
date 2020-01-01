import Control.Monad.RWS
import Data.Sequence

type Intcode = [Int] -- Seq Int
type Machine = RWS [Int] [Int] (Intcode, Int)

main :: IO ()
main = putStrLn "hI"
