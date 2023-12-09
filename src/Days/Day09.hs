module Days.Day09 where
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 114 2

type Input = [[Int]]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (map read . words) . lines

part1 :: Input -> Output1
part1 = solve . map reverse

solve :: [[Int]] -> Int
solve = sum . map extrapolate
    where
        extrapolate :: [Int] -> Int
        extrapolate xs
            | all (==0) diffs = head xs
            | otherwise = head xs - extrapolate diffs
            where diffs = zipWith subtract xs (tail xs)

part2 :: Input -> Output2
part2 = solve
