module Days.Day06 where
import           Data.List       (transpose)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 288 71503

type Input = [[String]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser =  map (tail . words) . lines

part1 :: Input -> Output1
part1 = product . map (uncurry numWins . listToTuple . map read) . transpose

numWins :: Int -> Int -> Int
numWins time minDist = right' - left' + 1
    where
        -- tx - x^2 > minDist
        -- x^2 - tx + minDist < 0
        discriminant :: Double
        discriminant = sqrt $ fromIntegral $ (time * time) - (4 * minDist)

        -- Compute roots and round them inwards
        left  = ceiling $ (fromIntegral time - discriminant) / 2
        right = floor   $ (fromIntegral time + discriminant) / 2

        -- Account for equality
        left'  = if left  * (time-left)  == minDist then left  + 1 else left
        right' = if right * (time-right) == minDist then right - 1 else right

part2 :: Input -> Output2
part2 = uncurry numWins . listToTuple . map (read . concat)
