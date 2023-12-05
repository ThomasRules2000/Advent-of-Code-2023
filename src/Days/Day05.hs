module Days.Day05 where
import           Data.Bifunctor   (bimap)
import           Data.Composition ((.:))
import           Data.List        (foldl', sort, sortOn, uncons)
import           Data.List.Split  (chunksOf, splitOn)
import           Data.Maybe       (fromJust)
import           Data.Tuple.Extra (snd3)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import           Util.Util        (listToTuple, listToTuple3)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 35 46

type Input = ([Int], [[RangeMapping]])

type RangeMapping = (Int, Int, Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (map read . words . drop 6) (map parseMapping) . fromJust . uncons . splitOn "\n\n"
    where
        parseMapping :: String -> [RangeMapping]
        parseMapping = sortOn snd3 . map (listToTuple3 . map read . words) . tail . lines

part1 :: Input -> Output1
part1 (nums, maps) = minimum $ map (`checkMaps` maps) nums

checkMaps :: Int -> [[RangeMapping]] -> Int
checkMaps = foldl' go
    where
        go :: Int -> [RangeMapping] -> Int
        go n [] = n
        go n ((destStart, srcStart, len):rest)
            | n >= srcStart && n < srcStart + len = destStart - srcStart + n
            | otherwise = go n rest

part2 :: Input -> Output2
part2 (xs, maps) = fst $ head $ foldl' (sort .: transformRanges) (sort $ map listToTuple $ chunksOf 2 xs) maps


transformRanges :: [(Int, Int)] -> [RangeMapping] -> [(Int, Int)]
transformRanges rs [] = rs
transformRanges [] _ = []
transformRanges rrs@(r@(rStart, rLen):rs) mms@((destStart, srcStart, mapLen):ms)
    | rStart >= srcStart + mapLen = transformRanges rrs ms     -- If our range starts after the end of this mapping, go to the next mapping
    | rStart + rLen <= srcStart   = r : transformRanges rs mms -- If our range ends before the start of this mapping, there is a gap so this range maps to itself
    | otherwise                   = prepend <> transformRanges carryForward mms
    where
        lRange@(_, lRangeLen) = (rStart, srcStart - rStart) -- Range from the left of our range to the start of the mapping, gap between ranges that maps to itself
        rRange@(_, rRangeLen) = (srcStart + mapLen, (rStart + rLen) - (srcStart + mapLen)) -- Range from the end of the mapping to the end of our range, carry over to next mapping

        overlapStart = max srcStart rStart
        overlapLen = min (srcStart + mapLen) (rStart + rLen) - overlapStart

        mappedRange = (overlapStart + (destStart - srcStart), overlapLen)

        prepend      = if lRangeLen > 0 then [lRange, mappedRange] else [mappedRange]
        carryForward = if rRangeLen > 0 then rRange:rs else rs
