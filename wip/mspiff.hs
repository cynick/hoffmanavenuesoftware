
import Prelude

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Aeson hiding (Array)
import Data.List
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import Data.Maybe
import Data.Array
import Control.Monad
import System.Environment

type FilmId = Int
type ScreeningId = Int
type Title = T.Text
type Showtime = Int
type Duration = Int
type Screen = T.Text

data Film = Film
  { filmId :: FilmId
  , filmTitle :: Title
   --  ... other attributes as they become interesting.
  }
  deriving (Eq,Show)

instance FromJSON Film where
  parseJSON (Object v) =
    Film <$>
      v .: "filmId" <*>
      v .: "filmTitle"

  parseJSON _ = error "invalid film json"

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , showtime :: Showtime
  , duration :: Duration
  }
  deriving (Eq, Show)

instance Ord Screening where
  compare a b = showtime a `compare` showtime b

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening <$>
      v .: "scFilmId" <*>
      v .: "screeningId" <*>
      v .: "screeningTime" <*> -- seconds since Epoch
      ((60*) <$> v .: "duration" ) -- duration is given in minutes
  parseJSON _ = error "invalid screening json"

loadFilms :: IO [Film]
loadFilms = load "wip/films"

loadScreenings :: IO [Screening]
loadScreenings = load "wip/screenings"

load :: FromJSON a => FilePath -> IO a
load path = do
  putStrLn $ "Loading " ++ path
  raw <- BS.readFile path
  case decode raw of
    Just r ->
      case fromJSON r of
        Success a -> do
          putStrLn $ "Finished loading " ++ path
          return a
        Error s -> error s
    Nothing -> error "Failed to parse"

newtype Schedule = Schedule { scheduleScreenings :: [Screening] }
  deriving (Eq,Show)

type WholeSchedule = Schedule
type ViewableSchedule = Schedule
type Catalog = [Film]

viewableSchedulesFor :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor ws films =
  map Schedule .
  filter disjoint .
  sequence $ screeningListsFor ws films

filmsInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsInSchedule cat (Schedule s) =
  catMaybes $ flip lookup fps <$> (scFilmId <$> s)
    where
      fps = zip (filmId <$> cat) cat

filmsNotInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsNotInSchedule cat vs = cat \\ (filmsInSchedule cat vs)

filmMissedBy :: WholeSchedule -> ViewableSchedule -> Film -> Bool
filmMissedBy ws (Schedule vs) film = all (not . disjoint) $ augmentedSchedules
  where
    augmentedSchedules = (:vs) `map` screeningsFor ws film

filmsMissedBy :: Catalog -> WholeSchedule -> ViewableSchedule -> [Film]
filmsMissedBy cat ws vs =
  filter (filmMissedBy ws vs) (filmsNotInSchedule cat vs)

screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor s f = filter (filt f) (scheduleScreenings s)
  where filt film screening = scFilmId screening == filmId film

screeningListsFor :: WholeSchedule -> [Film] -> [[Screening]]
screeningListsFor = map . screeningsFor

after :: Screening -> Screening -> Bool
after a b = showtime a > showtime b + duration b

disjoint :: [Screening] -> Bool
disjoint = not . any overlaps . pairsOf
  where
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]
    overlaps (a, b) = not (a `after` b || b `after` a)

{--
type FilmFilter = Film -> Bool

filmFilter :: FilmFilter -> [Film] -> [FilmId]
filmFilter filt films = map filmId . filter filt $ films

filmsForToday :: [Film] -> [FilmId]
filmsForToday = filmFilter (\_ -> True)

--}

{-
sequence' :: OverlapMatrix -> [[Screening]] -> [[Screening]]
sequence' mat = foldr lift' ([[]])
  where
    lift' l acc = do
      a <- l
      b <- acc
      guard (g a b)
      return (a:b)
    g a (b:_) = mat ! (screeningId a,screeningId b)
    g _ _ = True
-}
  
{--      a <- l
      b <- acc
      return ((:) a b)
--}   
-- liftM2 f m1 m2          = do { x1 <- m1; x2 <- m2; return (f x1 x2) }

{-
sequence = do
  a <- x
  b <- y
  c <- z
  guard (disjoint [a,b,c])
  return [a,b,c]
  -}

showtimesForSchedule :: ViewableSchedule -> [UTCTime]
showtimesForSchedule = (toUtc <$>) . sort . scheduleScreenings
  where
    toUtc = posixSecondsToUTCTime . fromIntegral . showtime

readInt :: String -> Int
readInt = read

{-
main :: IO ()
main = do
  s <- DL.sort <$> loadScreenings
  let mat = overlapMatrix s  
  (c:_) <- getArgs
  let w = Schedule s 
  f <- loadFilms
  let (sc1:_) = DL.take 1 $ DL.dropWhile (not . disjoint' mat) $ sequence $ screeningListsFor w (DL.take (readInt c) f)
  mapM_ print sc1
-}

  
  
  
{-
type OverlapMatrix = Array (Int,Int) Bool
overlapMatrix :: [Screening] -> OverlapMatrix
overlapMatrix ss =
  array ((0,0),(len,len))
        [ ((screeningId a,screeningId b), a `overlaps` b)
        | a<- ss, b <- ss
        ]
  where
    len = length ss -1
    overlaps a b = not (a `after` b || b `after` a)

disjoint'' :: [Screening] -> Bool
disjoint'' s =
  (len * (len - 1)) == length [() | a <- s, b <- s, not (a `overlaps` b)]
  where
    len = length s
    overlaps a b = not (a `after` b || b `after` a)

disjoint' :: Array (Int,Int) Bool -> [Screening] -> Bool
disjoint' mat = not . any overlaps . pairsOf
  where
    overlaps (a,b) = mat ! (screeningId a, screeningId b)
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]


-}
