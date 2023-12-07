module Days.Day07 where
import           Control.Arrow   ((>>>))
import           Control.DeepSeq (NFData)
import           Data.Bifunctor  (bimap)
import           Data.Function   (on)
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
testDay = T.testDay parser part1 part2 6440 5905

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show, Generic, NFData)

data Score = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show)

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
part1 = solve id compare

solve :: ([Card] -> [Card]) -> (Card -> Card -> Ordering) -> [([Card], Int)] -> Int
solve filterFunc comparator = sum . zipWith (*) [1..] . map snd . sortBy (compareHands filterFunc comparator `on` fst)

compareHands :: ([Card] -> [Card]) -> (Card -> Card -> Ordering) -> [Card] -> [Card] -> Ordering
compareHands filterFunc comparator h1 h2 = case (compare `on` (scoreHand . filterFunc)) h1 h2 of
        EQ -> head $ dropWhile (==EQ) $ zipWith comparator h1 h2
        x  -> x

part2 :: Input -> Output2
part2 = solve (filter (/=Jack)) cmpJokerWorst

cmpJokerWorst :: Card -> Card -> Ordering
cmpJokerWorst Jack Jack = EQ
cmpJokerWorst Jack _    = LT
cmpJokerWorst _ Jack    = GT
cmpJokerWorst x y       = compare x y

scoreHand :: [Card] -> Score
scoreHand = sort >>> group >>> map length >>> sort >>> \case
    []           -> FiveOfAKind  -- Hand of Jokers
    [_]          -> FiveOfAKind  -- Make all Jokers behave as single other card
    [1, _]       -> FourOfAKind  -- If the lowest number of another card type is 1, we can make 4 of a kind
    [_, _]       -> FullHouse    -- Otherwise this must be 2, so we make a full house
    [1, 1, _]    -> ThreeOfAKind -- If we have 3 types of card and 1 each of the two smallest, we can make 3 of a kind
    [_, _, _]    -> TwoPair      -- Otherwise we must already have a 2 pair
    [_, _, _, _] -> OnePair      -- Exactly 4 different types of card can only produce a single pair
    _            -> HighCard     -- This must be exactly 5 types of card
