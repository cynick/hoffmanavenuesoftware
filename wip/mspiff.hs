
import Prelude

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as S
import Data.Aeson hiding (Array)
import Data.List
import Data.List.Split
import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as DL
import qualified Control.Monad.Logic as L
import Data.Maybe
import Data.Array
import Control.Monad
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
  , filmScreenings :: [Screening]
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

arbitraryDistinctList :: (Arbitrary a, Eq a) => Int -> Gen [a]
arbitraryDistinctList count = do
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
    arbitrary = FilmList <$> arbitraryDistinctList 20

newtype DisjointList = DisjointList {fromScreeningList :: [Screening]}
  deriving Show

getOneDisjoint :: Gen [Screening]
getOneDisjoint = do
  l <- arbitraryDistinctList 20
  if disjoint l then return l else getOneDisjoint

instance Arbitrary DisjointList
  where
    arbitrary = DisjointList <$> getOneDisjoint

instance FromJSON Film where
  parseJSON (Object v) =
    Film <$>
      v .: "filmId" <*>
      v .: "filmTitle" <*>
      pure []

  parseJSON _ = error "invalid film json"

data ScreeningStatus =
  Unscheduled | Scheduled | Impossible | OtherScheduled
  deriving (Enum, Show)

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , otherScreening :: Maybe Screening
  , showtime :: Showtime
  , duration :: Duration
  , screen :: Screen
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
      v .: "screen"

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
screenings = toArray ((computeDeps . DL.sort) <$> loadScreenings)
  where
    computeDeps ss =
      let
        other s s' = s /= s' && scFilmId s' == scFilmId s
        set s = s { overlapping = fmap snd . filter overlaps . fmap (s,) $ (ss \\ [s])
                  , otherScreening = find (other s) ss
                  }

      in set <$> ss


films :: Array Int Film
films = toArray (computeDeps <$> loadFilms)
  where
    computeDeps fs =
      let
        schedule = Schedule (elems screenings)
        set f = f { filmScreenings = screeningsFor schedule f }
      in set <$> fs

filmOf :: Screening -> Film
filmOf s = fromJust $ DL.find (\f -> filmId f == scFilmId s) (elems films)

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

data MarkedScreening = MarkedScreening
  { status :: ScreeningStatus
  , markedScreening :: Screening
  , isPinned :: Bool
  , isRuledOut :: Bool
  }

instance Eq MarkedScreening where
  a == b = markedScreening a == markedScreening b

mkMarkedScreening :: Screening -> MarkedScreening
mkMarkedScreening s = MarkedScreening Unscheduled s False False

newtype Schedule = Schedule { scheduleScreenings :: [Screening] }
  deriving (Eq,Show,Monoid)

type WholeSchedule = Schedule
type DaySchedule = Schedule
type ViewableSchedule = Schedule
type Catalog = [Film]

showtimeToUtc :: Screening -> UTCTime
showtimeToUtc = posixSecondsToUTCTime . fromIntegral . showtime

dayOf :: Screening -> Day
dayOf = utctDay . showtimeToUtc

partitionByDay :: WholeSchedule -> [DaySchedule]
partitionByDay (Schedule s) = fmap Schedule $ reverse $ go s [] []
  where
    go [] curr ret = curr:ret
    go (x:xs) [] ret = go xs [x] ret
    go (x:xs) ys ret =
      if dayOf (last ys) /= dayOf x
       then go xs [x] (ys:ret)
       else go xs (x:ys) ret

type ScheduleState = M.Map Film (MarkedScreening,Maybe MarkedScreening)

addScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
addScreening ws s st =
  let
    f = filmOf s
    ms =
      mkMarkedScreening s
      { status = Scheduled
      , isPinned = False
      , isRuledOut = False
      }
    ms' =
      maybe Nothing (\s' -> mkMarkedScreening s'
        { status = OtherScheduled
        , isPinned = False
        , isRuledOut = False
        }) (otherScreening s)
  in M.insert f (ms,ms')

pinScreening ::
  Screening ->
  ScheduleState ->
  ScheduleState
pinScreening ws s st =
  let
    f = filmOf s
    ms =
      mkMarkedScreening s
      { status = Scheduled
      , isPinned = True
      , isRuledOut = False
      }
    ms' =
      maybe Nothing (\s' -> mkMarkedScreening s'
        { status = OtherScheduled
        , isPinned = False
        , isRuledOut = True
        }) (otherScreening s)
  in M.insert f (ms,ms')



updateSchedule ::
  Catalog ->
  WholeSchedule ->
  [MarkedScreening] -> [MarkedScreening]
updateSchedule cat ws mss = ret
  where
    schedule = viewableScheduleFor ws ms
    missed = maybe [] (filmsMissedBy cat ws) schedule
    screeningsOfMissed = concatMap filmScreenings missed
    ret = maybe ms rebuild schedule
    rebuild ms = DL.foldl' update [] mss
    update a m =
      case () of
       markedScreening ms `DL.elem` schedule = ms : a
       markedScreening ms `DL.notElem` schedule = ms { status 

viewableScheduleFor ::
  WholeSchedule ->
  [MarkedScreening] ->
  Maybe ViewableSchedule
viewableScheduleFor ws mss = listToMaybe schedules
  where
    schedules = filter (not . null) . filter disjoint . sequence $ lists
    screeningListFor ms lists =
      let
        screening = markedScreening ms
        status = screeningStatus ms
        list = [screening]
        ret | isPinned ms = list
            | isRuledOut ms = []
            | otherwise = maybe list (:list) (otherScreening screening)
      in ret : lists

    lists = foldr screeningListFor [] mss

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
    stitch [x,y] =
      let r = (x++y)
      in if disjointLists x y
           then r
           else []

    start = (filter disjoint . sequence) <$> chunksOf 2 (screeningListsFor ws fs)
    reduce :: [[[Screening]]] -> [[[Screening]]]
    reduce [] = []
    reduce [x] = [x]
    reduce xs = reduce (f (chunksOf 2 xs))

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

pairsOf' s = [(x,y) | (x:xs) <- tails s, y <- xs]
pairsOf s = [(a,b) | a <- s, b <- s, a/=b]

disjoint :: [Screening] -> Bool
disjoint = not . any overlaps . pairsOf
  where
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]

disjointLists' :: [Screening] -> [Screening] -> Bool
disjointLists' x y = disjoint (x++y)


disjointLists :: [Screening] -> [Screening] -> Bool
disjointLists = go
  where
    anyOverlap [] _ = False
    anyOverlap (y:ys) x | x `elem` overlapping y = True
                        | otherwise = anyOverlap ys x
    go [] [] = True
    go _ [] = True
    go [] _ = True
    go x y = not (foldr (\a b -> b || anyOverlap x a) False y)


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

impossiblePairs :: WholeSchedule -> [Film] -> [[Film]]
impossiblePairs w fs = DL.foldr filt [] combos
  where
    combos = [[a,b] | a <- fs, b <- fs, filmId a < filmId b]
    filt a b = if DL.null (viewableSchedulesFor' w a) then a:b else b

impossibleTriples :: WholeSchedule -> [Film] -> [[Film]]
impossibleTriples w fs = DL.foldr filt [] combos
  where
    combos = [[a,b] | a <- fs, b <- fs, c <- fs, filmId a < filmId b && filmId b < filmId c]
    filt a b = if DL.null (viewableSchedulesFor' w a) then a:b else b
