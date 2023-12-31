module Days.Day02 where
import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (bimap, first)
import           Data.Char       (isDigit)
import           Data.Foldable   (foldMap')
import           Data.List.Split (splitOn, splitOneOf)
import           GHC.Generics    (Generic)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 8 2286

data Bag = Bag {
    red   :: Int,
    green :: Int,
    blue  :: Int
} deriving (Show, Generic, NFData)

instance Semigroup Bag where
    a <> b = Bag {red   = max (red a)   (red b),
                  green = max (green a) (green b),
                  blue  = max (blue a)  (blue b)}

instance Monoid Bag where
    mempty = Bag 0 0 0

type Game = (Int, Bag)

type Input = [Game]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map getGame . lines
    where
        getGame :: String -> Game
        getGame = bimap (read . filter isDigit) (foldMap' (uncurry getCube . first read . listToTuple . words) . splitOneOf ";,")
                . listToTuple
                . splitOn ": "

        getCube :: Int -> String -> Bag
        getCube num "red"   = mempty{red   = num}
        getCube num "green" = mempty{green = num}
        getCube num "blue"  = mempty{blue  = num}
        getCube _   cube    = error $ "Bad cube: " <> cube

part1 :: Input -> Output1
part1 = sum . map fst . filter (\(_, Bag{..}) -> red <= 12 && green <= 13 && blue <= 14)

part2 :: Input -> Output2
part2 = sum . map (\(_, Bag{..}) -> red * green * blue)
