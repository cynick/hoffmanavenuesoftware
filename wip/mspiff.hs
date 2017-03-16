
import Prelude

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import Data.Aeson hiding (Array)
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import Data.Maybe
import Data.Array
import Control.Monad
import qualified Control.Monad.Logic as L
import System.Environment
import System.IO.Unsafe
import Test.QuickCheck hiding (Success)

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
  deriving (Show,Ord)

instance Eq Film where
  a == b = filmId a == filmId b

instance Arbitrary Film
  where
    arbitrary = do
      i <- choose (0, length films - 1)
      return (films ! i)

arbitraryFilmList :: Int -> Gen [Film]
arbitraryFilmList count = do
  let
    getOne list = do
      f <- arbitrary
      if f `elem` list then getOne list else return f
    build c list = do
      f <- getOne list
      if c == 0
        then return list
        else build (c -1) (f:list)
  build (min count (DL.length films)) []

newtype FilmList = FilmList {fromFilmList :: [Film]} deriving Show
instance Arbitrary FilmList
  where
    arbitrary = FilmList <$> arbitraryFilmList 50

newtype DisjointList = DisjointList {fromScreeningList :: [Screening]}
  deriving Show

getOneDisjoint :: Gen [Screening]
getOneDisjoint = do
  l <- arbitrary
  if disjoint l then return l else getOneDisjoint

instance Arbitrary DisjointList
  where
    arbitrary = DisjointList <$> getOneDisjoint

instance FromJSON Film where
  parseJSON (Object v) =
    Film <$>
      v .: "filmId" <*>
      v .: "filmTitle"

  parseJSON _ = error "invalid film json"

data ScreeningStatus =
  InSchedule | Impossible | OtherInSchedule | Unscheduled
  deriving (Enum, Show)

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , otherScreening :: Maybe Screening
  , showtime :: Showtime
  , duration :: Duration
  , screen :: Screen
  , status :: ScreeningStatus
  }
  deriving (Show)

instance Eq Screening where
  a == b = screeningId a == screeningId b

instance Ord Screening where
  compare a b = showtime a `compare` showtime b

instance Arbitrary Screening
  where
    arbitrary = do
      i <- choose (0, length screenings - 1)
      return (screenings ! i)

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening <$>
      v .: "scFilmId" <*>
      v .: "screeningId" <*>
      pure [] <*>
      pure Nothing <*>
      v .: "screeningTime" <*> -- seconds since Epoch
      ((60*) <$> v .: "duration" ) <*> -- duration is given in minutes
      v .: "screen" <*>
      pure Unscheduled
  parseJSON _ = error "invalid screening json"

loadFilms :: IO [Film]
loadFilms = load "wip/films"

loadScreenings :: IO [Screening]
loadScreenings = load "wip/screenings"

toArray :: IO [e] -> Array Int e
toArray m = unsafePerformIO $ do
  l <- m
  return $ array (0, DL.length l -1) (DL.zip [0..] l)

ws = Schedule (elems screenings)
screenings :: Array Int Screening
screenings = toArray (fmap (computeDeps . DL.sort) loadScreenings)

films :: Array Int Film
films = toArray loadFilms

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
  deriving (Eq,Show,Monoid)


type WholeSchedule = Schedule
type DaySchedule = Schedule
type ViewableSchedule = Schedule
type Catalog = [Film]

screeningShowtime :: Screening -> UTCTime
screeningShowtime = posixSecondsToUTCTime . fromIntegral . showtime

dayOf :: Screening -> Day
dayOf = utctDay . screeningShowtime

viewableSchedulesFor :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor ws fs =
  map Schedule .
  filter (not . null) .
  filter disjoint .
  sequence $ screeningListsFor ws fs

viewableSchedulesFor' :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor' ws fs = map Schedule $ filter (not.null) $ DL.concat $ reduce start
  where
    f = DL.foldr ((:) . g) []
    g :: [[[Screening]]] -> [[Screening]]
    g = filter (not . null) . fmap stitch . sequence
    stitch [x] = if disjoint x then x else []
    stitch [x,y] = if disjointLists x y then x ++ y else []

    start = (filter disjoint . sequence) <$> chunksOf 2 (screeningListsFor ws fs)
    reduce :: [[[Screening]]] -> [[[Screening]]]
    reduce [] = []
    reduce [x] = [x]
    reduce xs = reduce (f (chunksOf 2 xs))

{-
viewableSchedulesFor'' ws fs = 
  L.sequence (screeningListsFor ws fs) >>= fmap filt
  where
    filt xs = do
      x <- xs
      L.guard (disjoint x)
      return x
-}

filmsInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsInSchedule cat (Schedule s) =
  catMaybes $ flip lookup fps <$> (scFilmId <$> s)
    where
      fps = zip (filmId <$> cat) cat

filmsNotInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsNotInSchedule cat vs = cat \\ filmsInSchedule cat vs

filmMissedBy :: WholeSchedule -> ViewableSchedule -> Film -> Bool
filmMissedBy ws (Schedule vs) film = all (not . disjoint) augmentedSchedules
  where
    augmentedSchedules = (:vs) `map` screeningsFor ws film

filmsMissedBy :: Catalog -> WholeSchedule -> ViewableSchedule -> [Film]
filmsMissedBy cat ws vs =
  filter (filmMissedBy ws vs) (filmsNotInSchedule cat vs)

screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor s f = sort $ filter (filt f) (scheduleScreenings s)
  where filt film screening = scFilmId screening == filmId film

screeningListsFor :: WholeSchedule -> [Film] -> [[Screening]]
screeningListsFor = map . screeningsFor

after :: Screening -> Screening -> Bool
after a b = showtime a > showtime b + duration b

overlaps :: (Screening, Screening) -> Bool
overlaps (a, b) = not (a `after` b || b `after` a)

computeDeps :: [Screening] -> [Screening]
computeDeps ss =
  let
    other s s' = s /= s' && scFilmId s' == scFilmId s
    set s =
      s { overlapping = fmap snd . filter overlaps . fmap (s,) $ (ss \\ [s])
        , otherScreening = find (other s) ss
        }

  in set <$> ss

disjoint :: [Screening] -> Bool
disjoint = not . any overlaps . pairsOf
  where
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]

disjointLists' :: [Screening] -> [Screening] -> Bool
disjointLists' x y = disjoint (x++y)


anyOverlap :: [Screening] -> Screening -> Bool
anyOverlap [] _ = False
anyOverlap (y:ys) x | x `elem` overlapping y = True
                    | otherwise = anyOverlap ys x
disjointLists :: [Screening] -> [Screening] -> Bool
disjointLists = go
  where
    disjoint' x y = not (foldr (\a b -> b || anyOverlap x a) False y)
    go [] [] = True
    go _ [] = True
    go [] _ = True
    go x y = disjoint' x y && disjoint' y x


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
showtimesForSchedule = (toUtc <$>) . DL.sort . scheduleScreenings
  where
    toUtc = posixSecondsToUTCTime . fromIntegral . showtime

readInt :: String -> Int
readInt = read

main :: IO ()
main = do
  (c:_) <- getArgs
  let w = Schedule (elems screenings)
  forever $ do
    ts <- getCurrentTime
    f <- replicateM (readInt c) (generate arbitrary) :: IO [Film]
    case DL.take 1 (viewableSchedulesFor' w f) of
      [x] -> print (showtimesForSchedule x)
      _ -> print "NONE"
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

makeHoles :: Eq a => [a] -> [[a]]
makeHoles xs = fmap (\x -> xs \\ [x]) xs

impossibleFilt w a b = if DL.null (viewableSchedulesFor' w a) then a:b else b

impossible c w = DL.foldr (impossibleFilt w) [] c

type Impossibles = [Film] -> WholeSchedule -> [[Film]]

impossible2 :: Impossibles
impossible2 fs = impossible combos
  where
    combos = [[a,b] | a <- fs, b <- fs, filmId a < filmId b]

impossible3 :: Impossibles
impossible3 fs = impossible combos
  where
    combos = [[a,b,c] | a <- fs, b <- fs, c <- fs, filmId a < filmId b && filmId b < filmId c]
