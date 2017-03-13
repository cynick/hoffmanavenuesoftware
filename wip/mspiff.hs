
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
  deriving (Show,Ord)

instance Eq Film where
  a == b = filmId a == filmId b
  
instance FromJSON Film where
  parseJSON (Object v) =
    Film <$>
      v .: "filmId" <*>
      v .: "filmTitle"

  parseJSON _ = error "invalid film json"

data Screening = Screening
  { scFilmId :: FilmId
  , screeningId :: ScreeningId
  , overlapping :: [Screening]
  , otherScreening :: Maybe Screening
  , showtime :: Showtime
  , duration :: Duration
  }
  deriving (Show)

instance Eq Screening where
  a == b = screeningId a == screeningId b

instance Ord Screening where
  compare a b = showtime a `compare` showtime b

instance FromJSON Screening where
  parseJSON (Object v) =
    Screening <$>
      v .: "scFilmId" <*>
      v .: "screeningId" <*>
      pure [] <*>
      pure Nothing <*>
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
  
viewableDaySchedulesFor :: [Film] -> DaySchedule -> [ViewableSchedule]
viewableDaySchedulesFor films s =
  map Schedule .
  filter disjoint .
  sequence $ screeningListsFor s films

viewableSchedulesFor :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor ws films =
  map Schedule .
  filter disjoint .
  sequence $ screeningListsFor ws films

--viewableSchedulesFor' :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor' ws films = map Schedule $ DL.concat $ reduce start
  where
    f = DL.foldr (((:) . g)) []
    g :: [[[Screening]]] -> [[Screening]]
    g = take 500 . fmap stitch . sequence
    stitch [] = []
    stitch [x] = if disjoint x then x else []
    stitch [x,y] = 
      if disjoint (x++y) -- disjointLists x y
        then x ++ y -- (error $ "NOT " ++ show (fmap screeningId x) ++ ":" ++ show (screeningId <$> y))
        else []
       
    start = (filter disjoint . sequence) <$> chunksOf 2 (screeningListsFor ws films)
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
filmsNotInSchedule cat vs = cat \\ (filmsInSchedule cat vs)

filmMissedBy :: WholeSchedule -> ViewableSchedule -> Film -> Bool
filmMissedBy ws (Schedule vs) film = all (not . disjoint) $ augmentedSchedules
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

computeDeps :: WholeSchedule -> WholeSchedule
computeDeps (Schedule ws) =
  let
    other s s' = s /= s' && scFilmId s' == scFilmId s
    set s =
      s { overlapping = fmap snd . filter overlaps . fmap (s,) $ (ws \\ [s])
        , otherScreening = find (other s) ws
        }
        
  in Schedule (set <$> ws)
  
disjoint :: [Screening] -> Bool
disjoint = not . any overlaps . pairsOf
  where
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]

--disjointLists :: [Screening] -> [Screening] -> Bool
disjointLists x y = not $ any (==True) $ 
  fmap (\y' -> any (==True) (fmap (\x' -> x' `elem` overlapping y') x)) y


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
