module Days.Day01 where
import           Data.Char        (isDigit)
import           Data.List        (inits, isPrefixOf, isSuffixOf, tails)
import           Data.Maybe       (mapMaybe)
import           Data.Tuple.Extra (dupe, first)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

type Input = [String]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = lines

part1 :: Input -> Output1
part1 = sum . map getNums

getNums :: String -> Int
getNums s = read [head ds, last ds]
    where ds = filter isDigit s

part2 :: Input -> Output2
part2 = sum . map findNums

findNums :: String -> Int
findNums s = firstNum * 10 + lastNum
    where
        numStrings = map (first show . dupe) [0..9] ++ [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)]
        checkNum :: [(String, Int)] -> (String -> String -> Bool) -> String -> Maybe Int
        checkNum [] _ _ = Nothing
        checkNum ((numString, numResult):xs) f s
            | f numString s = Just numResult
            | otherwise = checkNum xs f s

        firstNum = head $ mapMaybe (checkNum numStrings isPrefixOf) (tails s)
        lastNum = head $ mapMaybe (checkNum numStrings isSuffixOf) (reverse $ inits s)
