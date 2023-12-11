module Days.Day11 where
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import qualified Util.Map        as Map

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

type Input = Set (Int, Int)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = Map.keysSet . Map.filter (=='#') . Map.fromGrid . lines

part1 :: Input -> Output1
part1 = solve 2

solve :: Int -> Set (Int, Int) -> Int
solve n gs = sum [manhattanDist g1 g2 | g1 <- expanded, g2 <- expanded, g1 < g2]
    where
        expanded =  map expand $ Set.toList gs

        emptyRows :: Set Int -> [Int]
        emptyRows xs = [x | x <- [Set.findMin xs..Set.findMax xs], x `Set.notMember` xs]

        expand :: (Int, Int) -> (Int, Int)
        expand (x, y) = (x + (n-1) * length (takeWhile (<x) $ emptyRows $ Set.map fst gs),
                         y + (n-1) * length (takeWhile (<y) $ emptyRows $ Set.map snd gs))

manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

part2 :: Input -> Output2
part2 = solve 1_000_000
