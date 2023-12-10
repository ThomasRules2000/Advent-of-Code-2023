module Days.Day10 where
import           Control.DeepSeq  (NFData)
import           Data.Bifunctor   (bimap)
import qualified Data.Map.Strict  as Map
import           Data.Maybe       (catMaybes, mapMaybe)
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
getPipe '|' = Set.fromAscList [North, South]
getPipe '-' = Set.fromAscList [East, West]
getPipe 'L' = Set.fromAscList [North, East]
getPipe 'J' = Set.fromAscList [North, West]
getPipe '7' = Set.fromAscList [South, West]
getPipe 'F' = Set.fromAscList [East, South]
getPipe _   = undefined

part1 :: Input -> Output1
part1 (start, pipes) = maximum $ fst <$> followLoop initialPoss pipes (Map.singleton start (0, startDirs)) 1
    where (initialPoss, startDirs) = findStarts start pipes

findStarts :: (Int, Int) -> Grid (Set Direction) -> ([((Int, Int), Direction)], Set Direction)
findStarts (x, y) grid = Set.fromList <$> unzip starts
    where
        starts = catMaybes [north, south, east, west]

        keepIfMember dir pos = do
            s <- grid Map.!? pos
            if Set.member dir s
                then return (pos, Set.findMin $ Set.delete dir s)
                else Nothing

        north = (,North) <$> keepIfMember South (x-1, y)
        south = (,South) <$> keepIfMember North (x+1, y)
        east  = (,East)  <$> keepIfMember West (x, y+1)
        west  = (,West)  <$> keepIfMember East (x, y-1)

followLoop :: [((Int, Int), Direction)] -> Grid (Set Direction) -> Grid (Int, Set Direction) -> Int -> Grid (Int, Set Direction)
followLoop currPoss pipes dists n
    | null newPoss = newDists
    | otherwise = followLoop newPoss pipes newDists $ n + 1
    where
        newPoss = mapMaybe newPosDir currPoss
        newDists = foldr (\(p, _) m -> Map.insert p (n, pipes Map.! p) m) dists currPoss

        newPosDir :: ((Int, Int), Direction) -> Maybe ((Int, Int), Direction)
        newPosDir ((x,y), dir) = if Map.member newPos dists then Nothing else Just (newPos, newDir)
            where
                newPos = case dir of
                    North -> (x-1, y)
                    South -> (x+1, y)
                    East  -> (x, y+1)
                    West  -> (x, y-1)
                newDir = Set.findMin $ Set.delete (next $ next dir) $ pipes Map.! newPos

part2 :: Input -> Output2
part2 (start, pipes) = (((maxX-minX+1) * (maxY-minY+1)) `div` 9) - Set.size (Set.filter (uncurry (&&) . both ((==0) . (`mod` 3))) $ Set.union loopTiles floodTiles)
    where
        (initialPoss, startDirs) = findStarts start pipes
        loopTiles = expandLoop $ snd <$> followLoop initialPoss pipes (Map.singleton start (0, startDirs)) 1

        floodTiles = floodOutside loopTiles minPos maxPos (Set.singleton minPos) Set.empty

        minPos@(minX, minY) = (subtract 1 $ Set.findMin $ Set.map fst loopTiles, subtract 1 $ Set.findMin $ Set.map snd loopTiles)
        maxPos@(maxX, maxY) = ((+1) $ Set.findMax $ Set.map fst loopTiles, (+1) $ Set.findMax $ Set.map snd loopTiles)

expandLoop :: Grid (Set Direction) -> Set (Int, Int)
expandLoop = Set.unions . Map.mapWithKey newTiles
    where
        newTiles :: (Int, Int) -> Set Direction -> Set (Int, Int)
        newTiles (x, y) dirs = Set.fromList $ catMaybes [Just newPos, north, south, east, west]
            where
                newPos@(newX, newY) = (x*3, y*3)
                north = if Set.member North dirs then Just (newX-1, newY) else Nothing
                south = if Set.member South dirs then Just (newX+1, newY) else Nothing
                east  = if Set.member East  dirs then Just (newX, newY+1) else Nothing
                west  = if Set.member West  dirs then Just (newX, newY-1) else Nothing

floodOutside :: Set (Int, Int) -> (Int, Int) -> (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
floodOutside loop minPos@(minX, minY) maxPos@(maxX, maxY) frontier closedSet
    | Set.null newFrontier = newClosed
    | otherwise = floodOutside loop minPos maxPos newFrontier newClosed
    where
        newClosed = Set.union closedSet frontier
        newFrontier = Set.unions $ Set.map newPoss frontier

        newPoss :: (Int, Int) -> Set (Int, Int)
        newPoss (x, y) = Set.filter validCandidate $ Set.fromAscList [(x-1, y), (x, y-1), (x, y+1), (x+1, y)]

        validCandidate :: (Int, Int) -> Bool
        validCandidate pos@(x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY && Set.notMember pos newClosed && Set.notMember pos loop
