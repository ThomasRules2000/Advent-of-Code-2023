module Days.Day05 where
import           Data.Bifunctor  (bimap)
import           Data.List       (uncons, foldl')
import           Data.List.Split (splitOn, chunksOf)
import           Data.Maybe      (fromJust)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple, listToTuple3)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 35 0

type Input = ([Int], [[(Int, Int, Int)]])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (map read . words . drop 6) (map parseMap) . fromJust . uncons . splitOn "\n\n"
    where
        parseMap :: String -> [(Int, Int, Int)]
        parseMap = map (listToTuple3 . map read . words) . tail . lines

part1 :: Input -> Output1
part1 (nums, maps) = minimum $ map (`checkMaps` maps) nums

checkMaps :: Int -> [[(Int, Int, Int)]] -> Int
checkMaps = foldl' go
    where
        go :: Int -> [(Int, Int, Int)] -> Int
        go n [] = n
        go n ((destStart, srcStart, len):rest)
            | n >= srcStart && n < srcStart + len = destStart - srcStart + n
            | otherwise = go n rest

part2 :: Input -> Output2
part2 (xs, maps) = minimum $ map (`checkMaps` maps) nums
    where
        nums = concatMap (\[x,y] -> [x..x+y]) $ chunksOf 2 xs
