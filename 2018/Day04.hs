import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import           Data.Set ( Set )
import qualified Data.Set as Set

import           Data.Ord       ( comparing )
import           Data.List      ( sort )
import           Data.Char      ( isDigit )
import           Data.Maybe     ( listToMaybe )
import           Data.Function  ( on )
import           Data.Foldable  ( maximumBy )
import           Data.Bifunctor ( bimap, second )

import           Data.Time.Clock    ( UTCTime (..) )
import           Data.Time.Calendar ( Day )

import           Data.Time.Format ( defaultTimeLocale, parseTimeM, formatTime )

import           Control.Monad ( forM_ )

import           Text.ParserCombinators.ReadP
--

main :: IO ()
main = do
  elines <- lines <$> readFile "Day04.input"

  case parseEvents elines of
    Nothing -> putStrLn "can't parse the events"
    Just es -> do
      let log      = eventLog es
      let ( g, m ) = second sleepiestMinute $ sleepiestGuard log
      putStrLn "Part1: "
      putStr "sleepiest guard: "
      print g
      putStr "sleepiest minute: "
      print m
      putStr "solution: "
      print $ g * m

      let ( g', m' ) = second sleepiestMinute $ mostFrequentGuard log
      putStrLn "Part2: "
      putStr "most frequent guard: "
      print g'
      putStr "most frequent minute: "
      print m'
      putStr "solution: "
      print $ g' * m'

--

type D         = Day
type H         = Word
type M         = Word
type ID        = Word
type TimeStamp = UTCTime

data Event
  = Shift TimeStamp ID
  | Sleep TimeStamp
  | Awake TimeStamp
  deriving ( Show )

instance Eq Event where
  (==) = (==) `on` getTimeStamp

instance Ord Event where
  compare = comparing getTimeStamp

type EventLog = Map ID ( Set Event )

--

eventP :: ReadP Event
eventP = shiftP <++ sleepP <++ awakeP

shiftP :: ReadP Event
shiftP = gEventP Shift <*> ( read <$> id )
  where id = string "Guard #" *> munch isDigit <* string " begins shift"

sleepP :: ReadP Event
sleepP = gEventP Sleep <* string "falls asleep"

awakeP :: ReadP Event
awakeP = gEventP Awake <* string "wakes up"

--

gEventP :: ( TimeStamp -> e ) -> ReadP e
gEventP e = e <$> ( timeStampP <* char ' ' )

timeStampP :: ReadP TimeStamp
timeStampP
  =   between ( char '[' ) ( char ']' ) ( many get )
  >>= maybe pfail pure . readTimeStamp
  where readTimeStamp = parseTimeM False defaultTimeLocale "%F %R"

--

parseEvent :: String -> Maybe Event
parseEvent = listToMaybe . fmap fst . readP_to_S eventP

parseEvents :: [ String ] -> Maybe ( Set Event )
parseEvents = fmap Set.fromList . sequence . fmap parseEvent

eventLog :: Set Event -> EventLog
eventLog = snd . Set.foldl go ( 0, mempty )
  where
    li ( Shift _ i ) = const i
    li _             = id

    go a e = let k = li e $ fst a in bimap
      ( const k )
      ( Map.alter ( Just . maybe ( Set.singleton e ) ( Set.insert e ) ) k )
      a

mxs :: ( M, M ) -> [ M ]
mxs ( l, h ) = filter ( \m -> l <= m && m < h ) [0..59]

sleepMinutes :: Set Event -> Map M Word
sleepMinutes
  = go mempty
  . concatMap mxs
  . pair
  . fmap ( getHourMinute . getTimeStamp )
  . Set.toList
  . Set.filter ( not . isShift )
  where go = foldr ( Map.alter ( Just . maybe 1 (+1) ) )

sleepiestGuard :: EventLog -> ( ID, Map M Word )
sleepiestGuard
  = maximumBy ( comparing $ sum . snd ) . Map.toList . fmap sleepMinutes

sleepiestMinute :: Map M Word -> M
sleepiestMinute = fst . maximumBy ( comparing snd ) . Map.toList

mostFrequentGuard :: EventLog -> ( ID, Map M Word )
mostFrequentGuard
  = maximumBy ( comparing $ cmp . snd ) . Map.toList . fmap sleepMinutes
  where cmp m = if Map.size m > 0 then maximum m else 0

--

getDay :: TimeStamp -> D
getDay = utctDay

getDayHour :: TimeStamp -> H
getDayHour = read . formatTime defaultTimeLocale "%H"

getHourMinute :: TimeStamp -> M
getHourMinute = read . formatTime defaultTimeLocale "%M"

pair :: [ a ] -> [ (a, a) ]
pair [] = []
pair [_] = []
pair ( x : y : xs ) = ( x, y ) : pair xs

getTimeStamp :: Event -> TimeStamp
getTimeStamp ( Shift t _ ) = t
getTimeStamp ( Sleep t   ) = t
getTimeStamp ( Awake t   ) = t

isShift :: Event -> Bool
isShift ( Shift _ _ ) = True
isShift _             = False
