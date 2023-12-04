module Days.Day03 where
import           Data.Bifunctor   (first, bimap)
import           Data.Char        (isDigit)
import           Data.Composition ((.:))
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (maybeToList)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import qualified Util.Map         as Map

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 4361 467835

type Input = (Map (Int, Int) Int, Map (Int, Int) Char)

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = buildMaps . lines

buildMaps :: [String] -> (Map (Int, Int) Int, Map (Int, Int) Char)
buildMaps = bimap Map.unions Map.unions . unzip . zipWith processLine [0..]
    where
        processLine :: Int -> String -> (Map (Int, Int) Int, Map (Int, Int) Char)
        processLine lineNo = first Map.fromAscList . flip go Nothing . zip [0..]
            where
                go :: [(Int, Char)] -> Maybe ((Int, Int), String) -> ([((Int, Int), Int)], Map (Int, Int) Char)
                go [] acc = (fmap (read . reverse) <$> maybeToList acc, mempty)
                go ((p, c):xs) acc
                    | isDigit c = go xs $ case acc of
                        Nothing       -> Just ((lineNo, p), [c])
                        Just (p', cs) -> Just (p', c:cs)
                    | otherwise = (fmap (read . reverse) <$> maybeToList acc, 
                                   if c == '.' then mempty else Map.singleton (lineNo, p) c) 
                                <> go xs Nothing

part1 :: Input -> Output1
part1 (numMap, symMap) = sum $ Map.filterWithKey (not . Set.null . Set.intersection (Map.keysSet symMap) .: getAround) numMap

getAround :: (Int, Int) -> Int -> Set (Int, Int)
getAround (x, y) n = Set.fromAscList $ [(x+x', y+y') | x' <- [-1..1], y' <- [-1..len], x' /= 0 || y' `elem` [-1, len]]
    where len = length $ show n

part2 :: Input -> Output2
part2 (numMap, symMap) = sum $ fmap product $ Map.filter ((==2) . length) $ Map.foldrWithKey' findGears mempty numMap
    where
        gearSet = Map.keysSet $ Map.filter (=='*') symMap
        findGears :: (Int, Int) -> Int -> Map (Int, Int) [Int] -> Map (Int, Int) [Int]
        findGears pos n gearMap = Map.unionWith (<>) gearMap
                                $ Map.fromSet (const [n])
                                $ Set.intersection gearSet
                                $ getAround pos n
