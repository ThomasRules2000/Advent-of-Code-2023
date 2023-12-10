module Days.Day10 where
import           Control.DeepSeq  (NFData)
import           Data.Bifunctor   (bimap)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (catMaybes)
import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Data.Tuple.Extra (both)
import           GHC.Generics     (Generic)
import qualified Program.RunDay   as R (runDay)
import qualified Program.TestDay  as T (testDay)
import           System.Clock     (TimeSpec)
import           Test.Hspec       (Spec)
import           Util.Cycle       (Cycle, next)
import qualified Util.Map         as Map
import           Util.Map         (Grid)
import           Util.Util        (toMaybe)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

data Direction = North | East | South | West
    deriving (Eq, Ord, Show, Generic, NFData, Enum, Bounded, Cycle)

type Input = ((Int, Int), Grid (Set Direction))

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = bimap (head . Map.keys) (fmap getPipe)
       . Map.partition (=='S')
       . Map.filter (/='.')
       . Map.fromGrid
       . lines

getPipe :: Char -> Set Direction
getPipe '|' = Set.fromDistinctAscList [North, South]
getPipe '-' = Set.fromDistinctAscList [East, West]
getPipe 'L' = Set.fromDistinctAscList [North, East]
getPipe 'J' = Set.fromDistinctAscList [North, West]
getPipe '7' = Set.fromDistinctAscList [South, West]
getPipe 'F' = Set.fromDistinctAscList [East, South]
getPipe _   = undefined

part1 :: Input -> Output1
part1 (start, pipes) = fst $ followLoop pipes $ fst $ findStarts start pipes

findStarts :: (Int, Int) -> Grid (Set Direction) -> ([((Int, Int), Direction)], Set Direction)
findStarts (x, y) grid = Set.fromDistinctAscList <$> unzip (catMaybes [north, east, south, west])
    where
        keepIfMember pos = fmap ((pos,) . Set.findMin)
                         . (Map.lookup pos grid >>=)
                         . Set.alterF (\case True->Just False; False->Nothing)

        north = (,North) <$> keepIfMember (x-1, y) South
        south = (,South) <$> keepIfMember (x+1, y) North
        east  = (,East)  <$> keepIfMember (x, y+1) West
        west  = (,West)  <$> keepIfMember (x, y-1) East

followLoop :: Grid (Set Direction) -> [((Int, Int), Direction)] -> (Int, Set (Int, Int))
followLoop pipes currPos = bimap (1+) (Set.union $ Set.fromList $ map fst currPos)
                          $ if p1 == p2 then (1, Set.singleton p1) else followLoop pipes newPoss
    where
        newPoss@[(p1, _), (p2, _)] = map newPosDir currPos

        newPosDir :: ((Int, Int), Direction) -> ((Int, Int), Direction)
        newPosDir ((x,y), dir) = (newPos, newDir)
            where
                newPos = case dir of
                    North -> (x-1, y)
                    South -> (x+1, y)
                    East  -> (x, y+1)
                    West  -> (x, y-1)
                newDir = Set.findMin $ Set.delete (next $ next dir) $ pipes Map.! newPos

part2 :: Input -> Output2
part2 (start, pipes) = ((maxX+1) * (maxY+1)) - uncurry (+) (both (Set.size . Set.filter (uncurry (&&) . both even)) (expandedLoopTiles, floodTiles))
    where
        (initialPoss, startDirs) = findStarts start pipes
        loopTiles = snd $ followLoop pipes initialPoss
        expandedLoopTiles = expandLoop $ Map.insert start startDirs $ Map.restrictKeys pipes loopTiles

        floodTiles = flood expandedLoopTiles (both ((+1) . (*2)) maxPos) (Set.singleton (-1, -1)) Set.empty

        maxPos@(maxX, maxY) = both (Set.findMax . (`Set.map` loopTiles)) (fst, snd)

expandLoop :: Grid (Set Direction) -> Set (Int, Int)
expandLoop = Set.unions . Map.mapWithKey newTiles
    where
        newTiles :: (Int, Int) -> Set Direction -> Set (Int, Int)
        newTiles (x, y) dirs = Set.fromDistinctAscList $ newPos : catMaybes [east, south]
            where
                newPos@(newX, newY) = (x*2, y*2)
                south = toMaybe (Set.member South dirs) (newX+1, newY)
                east  = toMaybe (Set.member East  dirs) (newX, newY+1)

flood :: Set (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
flood loop bounds@(maxX, maxY) frontier closedSet
    | Set.null newFrontier = newClosed
    | otherwise = flood loop bounds newFrontier newClosed
    where
        newClosed = Set.union closedSet frontier
        newFrontier = Set.unions $ map newPos $ Set.toList frontier

        newPos :: (Int, Int) -> Set (Int, Int)
        newPos (x, y) = Set.fromDistinctAscList $ filter validCandidate [(x-1, y), (x, y-1), (x, y+1), (x+1, y)]

        validCandidate :: (Int, Int) -> Bool
        validCandidate pos@(x, y) = x >= -1 && x <= maxX
                                 && y >= -1 && y <= maxY
                                 && Set.notMember pos newClosed
                                 && Set.notMember pos loop
