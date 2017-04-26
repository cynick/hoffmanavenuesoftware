---
author: cynick
date: 2013-12-10 09:48:35+00:00
layout: post
link: http://hoffmanavenuesoftware.com/thinking-functionally
slug: thinking-functionally
title: Thinking Functionally
---

I recently ported my leisure blog over to Hakyll, and said goodbye
to WordPress.
In doing so, I wrote a navigation feature I thought might give
some teachable ideas to someone just getting their feet wet with
functional programming.

Instead of using the pagination facilities offered by Hakyll,
I just wanted a simple prev/next scheme, ie., give each post
"previous" and "next" links that (respectively) take the reader
to the previous post, or the next post.

This boils down to the problem of writing a function that,
given an item in a list, returns the item on either side of that item.

First we'll come up with a type for this function.

Supposing that have a <code>Post</code> type, given a
list of <code>Post</code>s, and the <code>Post</code> we are currently
interested in, we want to return a pair of <code>Post</code>s:

``` haskell
findAdjacent :: [Post] -> Post -> (Post,Post)
```

There are three edge conditions to consider:<br>
1. the given post does not have a predecessor<br>
2. the given post does not have a successor<br>
3. the given post is not in the given lists of posts.<br>

We can use the <code>Maybe</code> type to codify the first two conditions:

``` haskell
findAdjacent :: [Post] -> Post -> (Maybe Post, Maybe Post)
```

For the last condition, we could wrap the whole result in an <code>Either</code> type

``` haskell
findAdjacent :: [Post] -> Post -> Either String (Maybe Post, Maybe Post)
```

where the <code>Left</code> side contains an error message, and
<code>Right</code> side contains a successful result.

However, this function will be used in a program that, when run,
generates a website. If the programmer tries to invoke <code>findAdjacent</code>
with an item that's not in the given list, the program should end
with an error.

So let's just stick with

``` haskell
findAdjacent :: [Post] -> Post -> (Maybe Post, Maybe Post)
```
Now, how do we implement this function?

First, maybe a fundamental observation.<br>
In functional programming, one often runs across the slogan that
"everything is a function", but this is not entirely accurate.
A better slogan is "functions and data are the same thing."

Let's think about that.<br>
What is a function, after all?<br>
A function is just a mapping from some domain to some range,
where an item from the domain maps to at most one item in the range.<br>
(Remember the 'vertical line test' from high school math?)<br>
Functions that map each item from the domain to *exactly* one item
in the range are called 'total functions', functions where an item
is left unmapped are called 'partial functions'.

Functions with domains that have finite cardinality can be expressed
directly with a lookup table, ie., data.
So one way to think about our problem is that we want to construct a
table whose keys are the given list of posts, where each post
maps to a pair of <code>Maybe Post</code>.<br>
We can then perform a lookup in that table to get the
answer we want for a particular item.

What is the type of this table?<br>
We will express it with a list of key/value pairs, but, if the list
of posts was rather large, we might think about using some kind of Map.

``` haskell
type AdjacencyTable = [(Post, (Maybe Post, Maybe Post))]
```

so we want a <code>buildTable</code> function of type

``` haskell
buildTable :: [Post] -> AdjacencyTable
```

What does <code>buildTable</code> look like?

Functional programming is more concerned with transforming data
than the specification an ordered set of instructions to be carried out.<br>
Here we want to somehow transform a list of posts into an <code>AdjecencyTable</code>.
Let's try to draw a picture of that transformation.

Suppose we have five posts:
``` code
  1 2 3 4 5
```

We know that the table values will be pairs whose members
are from the input list of posts, so let's write down those lists again:

``` code
  1 2 3 4 5
  1 2 3 4 5
  1 2 3 4 5
```

Now we want to start thinking about how to transform the first list
into something that looks like a list of what we're trying to find,
ie., a list of tuples.

If we move the latter two rows around a little, we can arrange it so that
the columns look like the results we're after:

``` code
  1 2 3 4 5
    1 2 3 4 5
1 2 3 4 5
```
Row 1 represents the original list.<br>
Row 2 represents the predecessors.<br>
Row 3 represents the successors.<br>
Eg., 1 and 3 are 2's predecessor and successor.<br>
We can also see that 1 doesn't have a predecessor, and 5 doesn't
have a successor.

Now we ask, how can we transform the first list into these new lists? <br>
We can make this easier by 'filling in' the holes with <code>Nothing</code>
(abbreviated with n), and imagining that the other values are wrapped in <code>Just</code>:

``` code
  1 2 3 4 5
n n 1 2 3 4 5
1 2 3 4 5 n n
```

We can see that the second row is just the original list
with a couple of Nothing values prepended to it:

``` haskell
predecessors = Nothing : Nothing : map Just posts
```

We can see that the third row is just the original
list with a couple of <code>Nothing</code> values appended:

``` haskell
successors = map Just posts ++ [Nothing,Nothing]
```

Since our final result will be made up of pairs of items from these lists
at each index, we make those pairs by zipping the lists together.
We also see that the first pair isn't useful, so we drop it.

``` haskell
adjPairs = drop 1 (zip predecessors successors)
```

So now we have a list of type <code>[(Maybe Post, Maybe Post)]</code>.

To get our table, we just need to zip this list with the original list:

``` haskell
adjTable = zip posts adjPairs
```

which has type <code>[(Post, (Maybe Post,Maybe Post))]</code>, as desired.

We also note that we've used <code>map Just posts</code> twice, so we might as
well share that expression.

Putting it all together:

``` haskell
buildTable :: [Post] -> [(Post, (Maybe Post, Maybe Post))]
buildTable posts = adjTable
  where
    posts' = map Just posts
    predecessors = Nothing : Nothing : posts'
    successors = posts' ++ [Nothing,Nothing]
    adjPairs = drop 1 (zip predecessors successors)
    adjTable = zip posts adjPairs
```

Now we're ready to write <code>findAdjacent</code>.
```haskell
findAdjacent :: [Post] -> Post -> (Maybe Post, Maybe Post)
findAdjacent posts post | post `elem` posts = go
                        | otherwise = err
  where
    go =
      let Just r = lookup post (buildTable posts)
      in r
    err = error $ "Expected " ++ show post ++ " to be a member of " ++ show posts
```

We use a guard to handle the case of bad input.
Ie., if the given post is an element of the posts list, we perform
the lookup.
If not, we err out with a message that the post wasn't in the list.

In the <code>go</code> function itself, note that we use an irrefutable pattern.
Such patterns will cause exceptions to be thrown if the pattern
doesn't match.<br>
In this case, we happen to know that since post is a member of posts, the
constructed table will have a key for post, and therefore, <code>lookup</code>
will always return a <code>Just</code>, so we can safely pattern match the
result pair out of the <code>Just</code>.
In real production code, it's usually a good idea to avoid such
patterns, and make the errors that can be caused by them to be more explicit.

It's also worth noting that <code>findAdjacent</code> only relies on <code>Post</code> having an <code>Eq</code>
instance (due to the use of the <code>lookup</code> function), and a <code>Show</code> instance
(due to <code>show</code> being invoked on the input post in the case of an error)
and not any particular attribute of the <code>Post</code> type.
Thus, we can rewrite the type of <code>findAdjacent</code> to work for any type that has
instances for <code>Show</code> and <code>Eq</code>.

``` haskell
findAdjacent :: (Show a, Eq a) => [a] -> a -> (Maybe a, Maybe a)
```

Playing around in GHCi, we see that this function works well:

``` code
λ: findAdjacent [1] 1
(Nothing,Nothing)

λ: findAdjacent [1,2] 1
(Nothing,Just 2)

λ: findAdjacent [1,2,3] 2
(Just 1,Just 3)

λ: let a = [1,2,3,4,5]
λ: mapM_ print $ fmap (\i -> (i, findAdjacent a i)) a
(1,(Nothing,Just 2))
(2,(Just 1,Just 3))
(3,(Just 2,Just 4))
(4,(Just 3,Just 5))
(5,(Just 4,Nothing))

```

Now that we have a working function, can we do better?

We could, for example, replace the <code>go</code> function above with
explicit recursion.

First, we pattern match on the empty list.
This isn't strictly needed, because we will only evaluate <code>go</code> if post
is an element of posts, which implies that posts is not empty, but
it's always a good idea to write total functions, and besides, GHC
will detect that this case is missing, via the incomplete-patterns
warning.<br>
This is a good thing!

``` haskell
go [] = (Nothing,Nothing)
```

Next, we pattern match on a singleton list:

``` haskell
go [_] = (Nothing,Nothing)
```

In the case of a singleton list, there is neither a predecessor or a
successor, so we return a pair of <code>Nothing</code>.

Now the case that the list has two elements.<br>
If the first element of the list is the given post, then it only
has a successor.<br>
If the second element of the list is the given post, then it only
has a predecessor.<br>
Otherwise, we drop down to a default that returns a pair of
<code>Nothing</code>, even though we know this case will never be hit.

``` haskell
go [x,y] | x == post = (Nothing, Just y)
         | y == post = (Just x, Nothing)
         | otherwise = (Nothing, Nothing)
```

Finally, the recursive case.<br>
We pattern match on the first three elements of the list.<br>
If x matches the post, then y is the post's successor.<br>
If y matches the post, then x is the post's predecessor and z is the
post's successor.<br>
Otherwise, we recursively evaluate <code>go</code> with the tail of the list.
The effect is that we slide a window of pattern matches down the
length of the list.
Eventually, one of the previous cases will match, and the recursion
will terminate with some result pair.

``` haskell
go xs'@(x:y:z:_) | x == post = (Nothing, Just y)
                 | y == post = (Just x, Just z)
                 | otherwise = go (tail xs')
```

Putting it together, we have
``` haskell
findAdjacent' :: (Show a, Eq a) => [a] -> a -> (Maybe a, Maybe a)
findAdjacent' posts post | post `elem` posts = go posts
                         | otherwise = err
  where
    go [] = (Nothing,Nothing)
    go [_] = (Nothing,Nothing)
    go [x,y] | x == post = (Nothing, Just y)
             | y == post = (Just x, Nothing)
             | otherwise = (Nothing, Nothing)
    go xs'@(x:y:z:_) | x == post = (Nothing, Just y)
                     | y == post = (Just x, Just z)
                     | otherwise = go (tail xs')

    err = error $ "Expected " ++ show a ++ " to be a member of " ++ show xs
```

We could test this function by hand just like we tested the original,
but since we already have a working function, a more efficient
approach is to use QuickCheck:

We need to write a predicate function that simply tests that the value
of <code>findAdjacent</code> and <code>findAdjacent'</code> match against random input:

``` haskell
prop1 :: (Show a, Eq a) => a -> [a] -> Bool
prop1 x xs = findAdjacent x xs == findAdjacent' x xs
```

Let's try it:
``` code
λ: import Test.QuickCheck
λ: quickCheck prop1

*** Failed! (after 1 test):
Exception:
  Expected () to be a member of []
  CallStack (from HasCallStack):
    error, called at src/foo.hs:10:11 in main:Foo
()
[]
```

Ah, QuickCheck evaluated prop with an empty list.
We want to filter those out.
Also, since we didn't specify a type for the elements,
QuickCheck chose the unit type, <code>()</code>.
We might feel better if we force it to choose Int or String.

The <code>(==>)</code> combinator allows us to write properties such that a given
condition holds on the input. It can be pronounced as 'implies', ie.,
a predicate on the test data being true 'implies' that the test can
be performed.
Test data that do not satisfy the predicate are rejected, and do
not figure into the outcome of the test.

In our case, we want to check that x is an element of the list,
but just for fun, let's also make sure that the list contains
no duplicates:

``` haskell
prop2 :: (Show a, Eq a) => a -> [a] -> Property
prop2 x xs = check ==> findAdjacent x xs == findAdjacent' x xs
  where check = x `elem` xs && nub xs == xs

λ: quickCheck prop2
*** Gave up! Passed only 40 tests.
```

Hrm, now what?
Apparently when generating arbitrary lists, lists containing no
duplicates are relatively rare, and therefore, the check function
filters out so many test cases that QuickCheck stops trying!

To avoid this, we can create a new data type for the test data we
need, and then write code that generates *exactly* the test cases we
want.
This is done by making our test data type an instance of the <code>Arbitrary</code> class:

``` haskell
data TestData a = TestData a [a] deriving (Show,Eq)

instance (Eq a, Arbitrary a) => Arbitrary (TestData a)
  where
    arbitrary = do
      xs <- nub `fmap` listOf1 arbitrary
      i <- choose (0, length xs - 1)
      return (TestData (xs !! i) xs)
```

We leave the choice of the embedded data type up to whatever code
uses it.
The <code>arbitrary</code> function here simply creates a random,
non-empty list of distinct a, and then picks a random integer to be an index
into that list, and then constructs a <code>TestData</code> object
with the appropriate values.

Now we can write

``` haskell
prop3 (TestData x xs) = findAdjacent x xs == findAdjacent' x xs
```

without using the <code>(==>)</code> combinator.

``` code
λ: quickCheck prop3
+++ OK, passed 100 tests.
```

That was fun, but now we can circle back and realize that if
we remove the nub xs == xs expression from the check function
in prop2, we'll also see 100 passing tests.



