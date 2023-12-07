module Days.Day04 where
import           Data.Bifunctor   (bimap)
import           Data.List.Split  (splitOn)
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (both)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import           Util.Util        (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 13 30

type Input = Map Int (Set Int, Set Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.fromList . map (bimap (read . drop 5) (both (Set.fromList . map read . words) . listToTuple . splitOn " | ") . listToTuple . splitOn ": ") . lines

part1 :: Input -> Output1
part1 = sum . fmap ((`div` 2) . (2^) . Set.size . uncurry Set.intersection)

part2 :: Input -> Output2
part2 cards = sum $ fmap snd $ foldl calcNewSums (fmap ((, 1) . Set.size . uncurry Set.intersection) cards) $ Map.keys cards
    where
        calcNewSums :: Map Int (Int, Int) -> Int -> Map Int (Int, Int)
        calcNewSums m n = Map.mapWithKey mapFun m
            where (wins, numCopies) = m Map.! n
                  mapFun :: Int -> (Int, Int) -> (Int, Int)
                  mapFun newN curr@(newWins, newCopies)
                    | newN <= n || newN > n + wins = curr
                    | otherwise = (newWins, newCopies + numCopies)
