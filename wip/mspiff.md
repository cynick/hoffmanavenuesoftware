---
author: cynick
date: 2013-12-10 09:48:35+00:00
layout: post
link: http://hoffmanavenuesoftware.com/analyzing-mspiff
slug: analyzing-mspiff
title: Analyzing the Minneapolis/St. Paul International Film Festival
---

I am a big fan of the Minneapolis/Saint Paul International Film
Festival.
For the past 17 years, I've purchased a Gold Pass and tried to 
see as many films as I could.
MSPIFF is very large, and it keeps getting bigger.

Typically, at any time during the festival, there are films
running on five screens at the same time.
The interesting twist is that most films usually have two screenings, 
which provides quite a bit of combinatorial complexity.
If each film screens twice, there are 2^(number of films) ways to 
see all of the films, a very large number indeed!

Now, while some people decide on all the films they want to see and obtain
all of their tickets a month in advance, the way I consume the
festival is to arrive on a given day and decide what I'm going to see that day.
This is because my moods change rather quickly.
Maybe when the time comes, I won't be interested in a emotionally
draining Iranian morality tale, but instead I'll prefer to see a
slapstick comedy from Chile.

This approach has caused problems for me, though.<br>
Suppose there are five films showing at some time slot.<br>
I have to choose one. <br>
One or more of the films I do *not* choose
might still be interesting to me, so, for those films, I have to
lookup the *other* time they may be showing, and then for each of
those screenings, look at what films are showing at the same time, 
and figure out what other times *those* films are screening.

After about three levels of this, the decision tree overflows the
tiny L1 cache of my brain.
And year after year, I inevitably get to the end of the festival 
only to realize that I had missed some film that I had particularily
wanted to see.

I needed some software to help me tame this decision tree!

I wanted two features:<br>
a) For a given (potentially large) list of films, calculate
all of the possible schedules wherein I could see all of those films
in sequence
and 
b) For each of the schedules provided by a), show me which films 
I would *miss* by choosing that schedule.


Let's get started.
First, we define the data types we'll need.

```haskell
import Data.Text

type FilmId = Int
type Title = Text
type Showtime = Int
type Duration = Int
data Screen = Text

data Film = Film 
  { filmId :: FilmId
  , filmTitle :: Title
   --  ... other attributes as they become interesting.
  } 
  deriving (Eq,Show)

data Screening = Screening 
  { scFilmId :: FilmId
  , showtime :: Showtime
  , duration :: Duration
  , screen   :: Screen
  } 
  deriving (Eq, Show)

newtype Schedule = Schedule { scheduleScreenings :: [Screening] } deriving Show

type WholeSchedule = Schedule
type ViewableSchedule = Schedule
type Catalog = [Film]
```

A <code>Schedule</code> is simply a list of <code>Screening</code>s.

A <code>WholeSchedule</code> will contain the schedule for every single 
screening in the festival; a <code>ViewableSchedule</code> will contain a
sequence of screenings that does not overlap.<br>
A Catalog represents all of the films that are screening in the festival.
We assume that functions of type <code>loadCatalog :: IO Catalog</code>
and <code>loadSchedule :: IO WholeSchedule</code>
are available.

Our first goal is to write a function that, given a
<code>WholeSchedule</code>, and a list of <code>Film</code>, 
we get a list of all of the possible ways to see all of the given films.

``` haskell
viewableSchedulesFor :: WholeSchedule -> [Film] -> [ViewableSchedule]
```

In doing this, we first need some utility functions.<br>

First, screening x is after screening y if the showtime of x
is after the end time of y, given by showtime + duration:<br>
(For a real-world solution, we might wish to add some padding
for travel between screening rooms, or a quick run to eat some food, 
but we'll ignore this for now.)

``` haskell 
after :: Screening -> Screening -> Bool
after x y = showtime x > showtime y + duration y
```

And then, two screenings x and y do not overlap if 
either x is strictly after y or y is strictly after x.

``` haskell
overlaps :: (Screening,Screening) -> Bool
overlaps (x,y) = not (x `after` y || y `after` x)
```

Next, given a list of screenings, we want to know whether or not 
they are disjoint, ie., none of the screenings overlap each other.

``` haskell
disjoint :: [Screening] -> Bool
disjoint s = not . any overlaps . pairsOf
  where 
    pairsOf s = [(a,b) | a <- s, b <- s, a/=b]
```
The definition of this function reads very much like the English
definition, which is pleasant.<br>
It's worth noting that laziness is a big help here.
The <code>any</code> function stops processing its input list when 
it first hits an element that makes its predicate true.<br>
Due to laziness, <code>pairsOf</code> is not 
constructed all in one go, but rather on demand as 
the <code>any</code> function consumes it.<br>
Ie., if the first two screenings in the list overlap, only one pair 
will be constructed.<br>
By the same token, we can see that if the pair (a,b)
overlaps, the pair (b,a) will overlap as well, but since
<code>any</code> short circuits, those extra pairs will not be considered.

Okay, now we need a function to give us all of the screenings of a
given film:

``` haskell
screeningsFor :: WholeSchedule -> Film -> [Screening]
screeningsFor (Schedule screenings) film = filter (filt film) screenings
  where filt film screening = scFilmId screening == filmId film
```
This is just a filter by comparing on the film id of the screening
to the film id of the film.

And then using this we can write a function that gives the list of screenings
for each film in a given film list:

``` haskell
screeningsForFilmList :: WholeSchedule -> [Film] -> [[Screening]]
screeningsForFilmList = map . screeningsFor
```

Now we're getting to the core of the problem.<br>
Given a <code>[[Screening]]</code>, we want to generate every 
combination of the items in each list,
and then filter out any combinations that are not disjoint.

We've already done something like this using list comprehension
syntax, but now we're a bit stymied since our input list of films
has arbitrary length.
Ie., if we had just two lists, we could write

``` haskell
combinations2 :: [Screening] -> [Screening] -> [[Screening]]
combinations2 x y = [ [a,b] | a <- x, b <- y]
```

and then if we had three lists

``` haskell
combinations3 :: [Screening] -> [Screening] -> [Screening] -> [[Screening]]
combinations3 x y z = [ [a,b,c] | a <- x, b <- y, c <- z]
```

ad nauseum.

Obviously, that won't work.<br>
It seems like what we need to do is iterate over the
<code>[[Screening]]</code>, and perform some action on each
<code>[Screening]</code>, and accumulate the results.<br>
In other words, a fold:

``` haskell
combinations :: [[a]] -> [[a]]
combinations scheduleList = foldr process ([[]]) scheduleList
```
Ie., we start with an empty list of lists, and fold the 'process'
function over the list of lists of <code>Screening</code>.

Looking at the type of <code>foldr</code>, we see that
<code>process</code> must have type
``` haskell
process :: [a] -> [[a]] -> [[a]]
```
Ie., it takes a list from the list of lists we are folding,
and the accumulated result, and returns a new accumulated result.<br>
What should this function do?

Earlier, we constructed the set of all pairs of elements from a given 
list via 

``` haskell 
pairsOf list = [(a,b) | a <- list, b <- list, a/=b]
```

We nearly want to do the same thing here, except that instead we now
want to combine all the elements of a list with all the elements of a 
*list of lists*.

``` haskell
process :: [a] -> [[a]] -> [[a]]
process list acc = [combine | x <- list, y <- acc]
```
In the above expression, <code>x :: a</code>, and <code>y :: [a]</code>, 
so the combine function needs to have type <code>a -> [a] -> [a]</code>.

One of the strengths of Haskell is that its type system allows one 
to reason about the *inhabitants* of a type (ie., for a function type,
implementations of that function), and sometimes there are so few 
inhabitants that only one really makes sense for what you're trying to do.

Since combine is a polymorphic pure function,
it lacks the type information needed to construct any new instances 
of a, so all we can do is manipulate the arguments we are given.<br>
We clearly want to use both arguments.<br>
Ie., we could return [] or just a list containing the first argument, 
or half of the elements of second argument,
but that would be discarding information when we're trying to 
build information.<br>
We could also imagine using some combination of repeat, cycle, and
take to create new lists of a with the given arguments, but that would
be adding *superfluous* information.

It really seems like the only thing that makes sense is to add 
the first argument to the second argument.<br>
There's a function for that, ie., the fundamental list constructor, 
which has the type we desire:

```haskell
(:) :: a -> [a] -> [a]
```

Let's try that.

``` haskell
combinations :: [[a]] -> [[a]]
combinations = foldr process ([[]])
  where process list acc = [x:y | x <- list, y <- acc]
```

Playing around in GHCi, we see that it does what we want.<br>
Neato.

``` code
λ: combinations [[1,2], [3,4]]
[[1,3],[1,4],[2,3],[2,4]]
λ: combinations [[1,2], [3,4], [5,6]]
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]
```

It was fun to reason about how to write this function, but it
shouldn't be too surprising that it already exists in the standard
library, generalized for all instances of Monad and Traversable, 
which [] has instances for both:

``` haskell
sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)
```

Okay!<br>
Now we can finally write 

``` haskell
viewableSchedulesFor :: WholeSchedule -> [Film] -> [ViewableSchedule]
viewableSchedulesFor schedule films = 
  map Schedule . filter disjoint . sequence $ screeningListsFor schedule films
```

Next up: Given a <code>ViewableSchedule</code>, are there films that
will be missed?

Once again, we need some utility functions.
For a given viewable schedule, we want a list of all the films 
that are in the catalog, but *not* in the given schedule.

``` haskell
filmsInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsInSchedule catalog (Schedule s) =
  catMaybes $ flip lookup fps <$> (scFilmId <$> s)
    where
      fps = zip (filmId <$> catalog) catalog

filmsNotInSchedule :: Catalog -> ViewableSchedule -> [Film]
filmsNotInSchedule catalog vs = catalog \\ (filmsInSchedule catalog vs)
```

Next we need to be able to ask, for a given film and a viewable
schedule, whether or not that film can be viewed.<br>
To do this, for each screening of a film we augment the given viewable schedule
with that screening.<br>
If none of these augmented schedules are disjoint, that means that all 
of the film's screenings overlap with some screening in the viewable
schedule, and therefore, the film will be missed.<br>

``` haskell
filmMissedBy :: WholeSchedule -> ViewableSchedule -> Film -> Bool
filmMissedBy ws (Schedule vs) film = all (not . disjoint) $ augmentedSchedules
  where 
    augmentedSchedules = (:vs) `map` screeningsFor ws film
```

And now we can write filmsMissedBy.<br>
It simply runs a filter by filmMissedBy over the films that are not in
the given viewable schedule.

```haskell
filmsMissedBy :: Catalog -> WholeSchedule -> ViewableSchedule -> [Film]
filmsMissedBy cat ws vs =
  filter (filmMissedBy ws vs) (filmsNotInSchedule cat vs)

```

With this, we can place a value on a given viewable schedule wherein 
the smaller the number of missed films associated with that schedule, 
the more valuable it is.
Ie., if we are choosing from all of the possible schedules of a list
of ten films, we'd like to pick the one that precludes us from seeing 
the fewest number of films *not* in the list, since that gives us more
options.
