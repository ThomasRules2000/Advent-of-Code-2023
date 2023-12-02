module Days.Day02 where
import           Data.Bifunctor  (bimap, first)
import           Data.Char       (isDigit)
import           Data.Foldable   (fold, foldMap')
import           Data.List.Split (splitOn)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 8 2286

data Cubes = Cubes {
    red   :: Int,
    green :: Int,
    blue  :: Int
} deriving Show

instance Semigroup Cubes where
    a <> b = Cubes {red   = max (red a)   (red b),
                    green = max (green a) (green b),
                    blue  = max (blue a)  (blue b)}

instance Monoid Cubes where
    mempty = Cubes 0 0 0

type Game = (Int, [Cubes])

type Input = [Game]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map getGame . lines
    where
        getGame :: String -> Game
        getGame = bimap (read . filter isDigit) (map getCubes . splitOn "; ")
                . listToTuple
                . splitOn ": "

        getCubes :: String -> Cubes
        getCubes = foldMap' (getCube . first read . listToTuple . words) . splitOn ", "

        getCube :: (Int, String) -> Cubes
        getCube (num, "red")   = mempty{red   = num}
        getCube (num, "green") = mempty{green = num}
        getCube (num, "blue")  = mempty{blue  = num}

part1 :: Input -> Output1
part1 = sum . map fst . filter ((\Cubes{..} -> red <= 12 && green <= 13 && blue <= 14) . fold . snd)

part2 :: Input -> Output2
part2 = sum . map ((\Cubes{..} -> red * green * blue) . fold . snd)
