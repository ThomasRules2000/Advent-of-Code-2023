module Days.Day07 where
import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (bimap)
import           Data.List       (group, sort, sortBy)
import           GHC.Generics    (Generic)
import qualified Program.RunDay  as R (runDay)
import qualified Program.TestDay as T (testDay)
import           System.Clock    (TimeSpec)
import           Test.Hspec      (Spec)
import           Util.Util       (listToTuple)

runDay :: String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay = R.runDay parser part1 part2

testDay :: String -> String -> Spec
testDay = T.testDay parser part1 part2 0 0

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show, Generic, NFData)

type Input = [([Card], Int)]

type Output1 = Int
type Output2 = Int

parser :: String -> Input
parser = map (bimap (map getCard) read . listToTuple . words) . lines

getCard :: Char -> Card
getCard '2' = Two
getCard '3' = Three
getCard '4' = Four
getCard '5' = Five
getCard '6' = Six
getCard '7' = Seven
getCard '8' = Eight
getCard '9' = Nine
getCard 'T' = Ten
getCard 'J' = Jack
getCard 'Q' = Queen
getCard 'K' = King
getCard 'A' = Ace
getCard _   = undefined

part1 :: Input -> Output1
part1 = sum . zipWith (*) [1..] . map snd . sortBy compareHands

scoreHand :: [Card] -> Int
scoreHand hand
    | groupSizes == [5]       = 7
    | groupSizes == [1, 4]    = 6
    | groupSizes == [2, 3]    = 5
    | groupSizes == [1, 1, 3] = 4
    | groupSizes == [1, 2, 2] = 3
    | groupSizes == [1,1,1,2] = 2
    | otherwise               = 1
    where groupSizes = sort $ map length $ group $ sort hand

compareHands :: ([Card], Int) -> ([Card], Int) -> Ordering
compareHands (h1, _) (h2, _)
    | h1 == h2 = EQ
    | h1Score > h2Score = GT
    | h1Score < h2Score = LT
    | otherwise = head $ filter (/=EQ) $ zipWith compare h1 h2
    where
        h1Score = scoreHand h1
        h2Score = scoreHand h2

part2 :: Input -> Output2
part2 = undefined
