{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Program.RunDay where
import           Config.Config          (year)
import           Control.DeepSeq        (NFData, force)
import           Control.Exception      (SomeException, evaluate, try)
import           Control.Monad.Extra    (unlessM)
import qualified Data.ByteString.Char8  as BS
import           Data.Either.Extra      (eitherToMaybe)
import           Data.Functor           (($>))
import           Formatting             (formatToString)
import           Formatting.Clock       (timeSpecs)
import           Network.HTTP.Simple    (addRequestHeader, getResponseBody,
                                         getResponseStatusCode, httpBS,
                                         parseRequest)
import           System.Clock           (Clock (Monotonic), TimeSpec, getTime)
import           System.Directory.Extra (doesFileExist)
import           System.Exit            (exitFailure)
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)
import           Util.ParserFunc        (ParserFunc, makeParser)

runDay :: (NFData inp, NFData out1, NFData out2, Show out1, Show out2, ParserFunc f inp)
       => f -> (inp -> out1) -> (inp -> out2) -> String -> IO (Maybe TimeSpec, Maybe TimeSpec, Maybe TimeSpec)
runDay parser part1 part2 filename = do
    -- Download the input if the file doesn't already exist
    unlessM (doesFileExist filename) $ downloadFile filename

    -- Read input file into memory to ensure this load isn't counted in the parse time
    file <- readFile filename >>= evaluate . force

    -- Time the parser, forcing the parse to complete
    parserStart <- getTime Monotonic
    parsed <- try $ (evaluate . force . makeParser parser) file
    parserEnd <- getTime Monotonic
    case parsed of
        Left (e :: SomeException) -> putStrLn "Unable to parse input!" >> print e $> (Nothing, Nothing, Nothing)
        Right input -> do
            let parserTime = parserEnd - parserStart
            putStrLn $ printf "Parser (%s)" $ formatToString timeSpecs parserStart parserEnd

            (p1Res, p1Time) <- runPart 1 part1 input
            (p2Res, p2Time) <- runPart 2 part2 input

            putStrLn $ printf "Total Time: %s" $ formatToString timeSpecs 0 (parserTime + p1Time + p2Time)

            return (Just parserTime, p1Res $> p1Time, p2Res $> p2Time)


runPart :: (NFData out, Show out) => Int -> (inp -> out) -> inp -> IO (Maybe out, TimeSpec)
runPart partNum partFunc input = do
    start <- getTime Monotonic
    res   <- try $ evaluate $ force $ partFunc input
    end   <- getTime Monotonic

    putStrLn $ printf "Part %d (%s):" partNum $ formatToString timeSpecs start end
    putStrLn $ either (\(e :: SomeException) -> printf "Unable to run Part %d!\n%s" partNum $ show e) show res

    return (eitherToMaybe res, end - start)

downloadFile :: String -> IO ()
downloadFile file = case readMaybe @Int $ take 2 $ drop 9 file of
    Nothing -> putStrLn (printf "Can't find file %s!" file) >> exitFailure
    Just n -> do
        key <- BS.readFile "sessionkey.txt"
        req <- addRequestHeader "Cookie" ("session=" <> key)
               <$> parseRequest (printf "https://adventofcode.com/%d/day/%d/input" year n)
        resp <- httpBS req
        case getResponseStatusCode resp of
            200    -> BS.writeFile file $ getResponseBody resp
            status -> do
                putStrLn $ printf "Unable to download %s from the server" file
                putStrLn $ printf "Response Code: %d" status
                putStrLn "Response: "
                BS.putStrLn $ getResponseBody resp
                exitFailure
