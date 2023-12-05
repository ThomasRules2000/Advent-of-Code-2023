module Days.Day05 where
import           Data.Bifunctor  (bimap)
import           Data.List       (uncons, foldl')
import           Data.List.Split (splitOn)
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

data Map = Map {
    source :: String,
    dest   :: String,
    ranges :: [(Int, Int, Int)]
}

type Input = ([Int], [Map])

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (map read . words . drop 6) (map parseMap) . fromJust . uncons . splitOn "\n\n"
    where
        parseMap :: String -> Map
        parseMap = (\((s, d), rs) -> Map s d rs) . bimap (listToTuple . splitOn "-to-" . head . words) (map (listToTuple3 . map read . words)) . fromJust . uncons . lines

part1 :: Input -> Output1
part1 (nums, maps) = minimum $ map (`checkMaps` maps) nums
    where
        checkMaps :: Int -> [Map] -> Int
        checkMaps = foldl' (\n -> checkMap n . ranges)

checkMap :: Int -> [(Int, Int, Int)] -> Int
checkMap n [] = n
checkMap n ((destStart, srcStart, len):rest)
    | n >= srcStart && n < srcStart + len = destStart - srcStart + n
    | otherwise = checkMap n rest

part2 :: Input -> Output2
part2 = undefined
