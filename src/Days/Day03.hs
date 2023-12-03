module Days.Day03 where
import           Data.Char       (isDigit)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import qualified Util.Map        as Map
import qualified Util.Set        as Set

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 4361 467835

data Schematic = Number Int | Symbol Char
    deriving (Eq, Ord, Show)

type Input = Map (Int, Int) Schematic

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = coalesceNums . Map.fromGrid . lines

coalesceNums :: Map (Int, Int) Char -> Map (Int, Int) Schematic
coalesceNums m = Map.mapMaybeWithKey getSchematic m
    where
        getSchematic :: (Int, Int) -> Char -> Maybe Schematic
        getSchematic _ '.' = Nothing
        getSchematic pos@(x, y) c
            | isDigit c = case Map.lookup (x, y-1) m of
                Just c' | isDigit c' -> Nothing
                _                    -> Just $ Number $ read $ getNum pos
            | otherwise = Just $ Symbol c

        getNum :: (Int, Int) -> String
        getNum pos@(x, y) = case Map.lookup pos m of
            Just c | isDigit c -> c : getNum (x, y+1)
            _                  -> ""

part1 :: Input -> Output1
part1 m = Map.foldrWithKey' addIfSymbol 0 m
    where
        addIfSymbol :: (Int, Int) -> Schematic -> Int -> Int
        addIfSymbol _ (Symbol _) acc = acc
        addIfSymbol pos (Number n) acc
            | any ((\case (Just (Symbol _)) -> True; _ -> False) . (`Map.lookup` m)) $ getAround pos n = acc + n
            | otherwise = acc

getAround :: (Int, Int) -> Int -> Set (Int, Int)
getAround (x, y) n = Set.fromList $ [(x, y-1), (x, y+len)] ++ [(x+x', y+y') | x' <- [-1, 1], y' <- [-1..len]]
    where len = length $ show n

part2 :: Input -> Output2
part2 m = sum $ fmap product $ Map.filter ((==2) . length) $ Map.foldrWithKey' findGears mempty m
    where
        findGears :: (Int, Int) -> Schematic -> Map (Int, Int) [Int] -> Map (Int, Int) [Int]
        findGears _   (Symbol _) gearMap = gearMap
        findGears pos (Number n) gearMap = Map.unionWith (<>) gearMap
                                         $ Map.fromSet (const [n])
                                         $ Set.filter ((== Just (Symbol '*')) . (`Map.lookup` m))
                                         $ getAround pos n
