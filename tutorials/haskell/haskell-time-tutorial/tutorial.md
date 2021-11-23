---
title: Time, patterns and a little more
published: 2021-11-25
tags: haskell, pattern-synonyms, property-testing
libraries: gtk3-0.14.4 glib-0.13.2.2
language: haskell
author: Felix Miño
author-name: Felix Miño
github-profile: felixminom
description: In this tutorial we’re going to explore the use of pattern synonyms in Haskell, later we’ll give a brief view to the Stack Builders’ contribution to the `time` Haskell library and finally we’ll review how to use the utilities that were introduced in the contribution.
---

As Stack Builder one of the core values of our company is contributing
to open source (OSS). We believe that we have the power to change
people’s lives (or at least make it easier) by pushing the boundaries
of the software industry and we’re happy to announce that the past month
we were able to contribute to one of Haskell's core libraries, time.

One of our tech leads, Sebastian Estrella, started with the idea:
“What if we bring some utilities that Ruby on Rails (RoR) has to haskell?”.
For instance, “How can I get the range of days that a quarter has?”
or “How can I get a month’s boundaries?” were some of the questions
that came to our mind. In the following example I will show the Haskell
code needed to get a month's boundaries and its version in RoR.

```haskell
type MonthFirstDay = Day
type MonthLastDay  = Day

monthBoundaries :: Day -> (MonthFirstDay, MonthLastDay)
monthBoundaries day = let (y, m , _) = toGregorian day
   in (fromGregorian y m 1, fromGregorian y m 31)
```

```ruby
irb(main):003:0> Date.today.all_week
=> Mon, 13 Sep 2021..Sun, 19 Sep 2021
```
elmsetting the last day to always be 31st if some months have
less than 31 days? Could be crossing your mind right now. Well that’s
because `fromGregorian` clip the values to be correct for each month,
but what is `fromGregorian`? Well that’s exactly what we wanted to
avoid when using the time library.

We wanted something more clear and readable as uncle bob mentioned:
"Indeed, the ratio of time spent reading versus writing is well over
10 to 1. We are constantly reading old code as part of the effort
to write new code. ...Therefore, making it easy to read makes it easier
to write.” So this was our motivation, to make something similar to the
utilities that RoR has and to ease Haskell developer’s lives.

We started by opening an [issue][issue] on Github. Quick parentheses here,
when you want to work on OSS I’d recommend to start opening an issue
in the repo and discuss/brainstorm your idea with the maintainers,
sometimes there are some good reasons why the feature you’re proposing
have not been yet implemented and if you open a PR directly it could
rejected, so save yourself a disappointment and open an issue first.
Back to our issue, the maintainer of the library surprisingly proposed
to implement these helper functions using pattern synonyms and I said
surprisingly because I haven’t heard about them, so that’s why I’ll
like to introduce you some basics about this Haskell feature that,
actually, it has been around for some years now.


## A little about pattern synonyms

First of all I’d like to start with a caveat, this guide is not meant
to explain pattern synonyms in depth, instead it’s meant to be a practical
introduction to this concept.

GHC documentation describe pattern synonyms like:

> “Pattern synonyms enable giving names to parametrized pattern schemes.
> They can also be thought of as abstract constructors that don’t have a
> bearing on data representation.”

And probably we’re in the same place as in the beginning so I’ve preferred
to introduce pattern synonyms with a real world example. We’re going to
borrow a pattern that exists in the time library. The [MonthOfYear][month-pattern]
pattern allows us to define months of the year not by a number but by its name.

The `MonthOfYear` data type is defined like this:

```haskell
type MonthOfYear = Int
```

As you can see, it is only an alias for `Int`. We now are able to define
months like `Int`s where January will correspond to 1 and so on. But
Fortunately, since GHC 7.8 we are able to use pattern synonyms and we
could, instead of using raw `Int`s, write this down in a pattern style
and have a more idiomatic way of defining months.

```haskell
pattern January :: MonthOfYear
pattern January = 1

pattern February :: MonthOfYear
pattern February = 2

pattern March :: MonthOfYear
pattern March = 3
.
.
.

-- | The twelve 'MonthOfYear' patterns form a @COMPLETE@ set.
pattern December :: MonthOf

{-# COMPLETE January, February, March, April, May, June, July, August, September, October, November, December #-}Year
pattern December = 12
```

If you’re curious about why the `{-# COMPLETE #-}` pragma is being
used here’s a [link][complete-pragma] to a complete explanation.

Now if we need to pass a `MonthOfYear` as a parameter to a function
we could write something like:

```haskell
foo November or foo 11 --Both will be equivalent
```

We’ll use this pattern in a function that basically takes a month and
a day and yields if the corresponding Date is a holiday.

```haskell
dateToHoliday :: MonthOfYear -> DayOfMonth -> String
dateToHoliday January  _   = "Happy new year!"
dateToHoliday November 1   = "Let's eat colada morada"
dateToHoliday December 25  = "Merry Christmas jojojo"
dateToHoliday _ _          = "Probably just a regular day"
```

Now you're probably wondering what is “colada morada”? Haha no worries,
it’s an ecuatorian tradition in Ecuador and we eat this meal that is
purple corn based with spices and fruits, pretty amazing to be honest.

Back to Haskell, I think it’s pretty clear what the `dateToHoliday`
function does, the first clause matches all days of January, second
one matches the All Saints holiday, the third one matches for Christmas
and the last one it’s the fallback for all other days. I think using
the `MonthOfYears` patterns makes it a lot more readable. Having something
like:

```haskell
dateToHoliday 12 25  = "Merry Christmas jojojo"
```

Will not be as straightforward as the function that we have right now.

I haven’t mentioned it yet, but the patterns that we built previously
are called bidirectional patterns, why? Because we can use them as
expressions as well, so something like this is totally valid.

``` bash
ghci> dateToHoliday January 12
"Happy new year!"
ghci> dateToHoliday November 1
"Let's eat colada morada"
```


So now we can use `11`, `November` in the `MonthOfYear` context
interchangeably across our code.


## Stack Builders contribution to Haskell’s time library

Well all of this preamble brings us to the contribution itself.
Pattern synonyms is what we used when working on `time`’s new features.
You can see the complete [pull request][PR-1](PR). I have to clarify
something, the PR has some different code than the one we’re going to
check here, since the maintainer decided to do some renaming and changed
the original PR’s code, you can check the changes [here][maintainer-PR].
Also, she added something extra to the boundaries that we had in mind at
first and now we can get the days that belong to a period and the period length.

First of all we started defining the new type class `DayPeriod`:

```haskell
class Ord p => DayPeriod p where
   -- | Returns the first 'Day' in a period of days.
   periodFirstDay :: p -> Day
   -- | Returns the last 'Day' in a period of days.
   periodLastDay :: p -> Day
   -- | Get the period this day is in.
   dayPeriod :: Day -> p
```

So after this one was defined all we had to do was create the instances
of the type class for the different data types: `Year`, `Quarter`, `Month`
and `Day` itself, so here it’s the code:

```haskell
instance DayPeriod Year where
   periodFirstDay y = YearMonthDay y January 1
   periodLastDay y = YearMonthDay y December 31
   dayPeriod (YearMonthDay y _ _) = y
```

Another pattern synonym is introduced here. The `YearMontDay` pattern lets
us build a `Day` in terms of the `Year`, `MonthOfYear` (do you remember
this one?) and `DayOfMOnth`, where all three are basically `Int`s. Here is
its definition:

```haskell
pattern YearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern YearMonthDay y m d <-
   (toGregorian -> (y, m, d))
   where
       YearMonthDay y m d = fromGregorian y m d
```

Now, let’s check the `Quarter` instance:

```haskell
instance DayPeriod Quarter where
   periodFirstDay (YearQuarter y q) =
       case q of
           Q1 -> periodFirstDay $ YearMonth y January
           Q2 -> periodFirstDay $ YearMonth y April
           Q3 -> periodFirstDay $ YearMonth y July
           Q4 -> periodFirstDay $ YearMonth y October
   periodLastDay (YearQuarter y q) =
       case q of
           Q1 -> periodLastDay $ YearMonth y March
           Q2 -> periodLastDay $ YearMonth y June
           Q3 -> periodLastDay $ YearMonth y September
           Q4 -> periodLastDay $ YearMonth y Dec
```

Which is defined in terms of the `periodFirstDay` and `periodLastDay` of a `Month`
and it uses (once again) a pattern named `YearMonth`.


Now let’s check the `Month` instance of `DayPeriod`:

```haskell
instance DayPeriod Month where
   periodFirstDay (YearMonth y m) = YearMonthDay y m 1
   periodLastDay (YearMonth y m) = YearMonthDay y m 31 -- clips to correct day
   dayPeriod (YearMonthDay y my _) = YearMonth y my
```

Apparently patterns are everywhere, and why not? They are pretty useful.

And last but not least, let's review the `Day` instance of `DayPeriod`:

```haskell
instance DayPeriod Day where
   periodFirstDay = id
   periodLastDay = id
   dayPeriod = id
```

As you could imagine, the first and last day of a day is the day itself.

### Some `time` new features examples.
So let's see what we can do with these new features introduced in the contribution.
In this section we will review some concrete code samples of how these new features
can help us in our daily software development routine. After making the proper imports
we can have something like this:

- To get starting and ending days of all quarters of a given year:

```haskell
type QuarterFirstDay = Day
type QuarterLastDay  = Day

allQuartersBoundaries :: Year -> [(QuarterFirstDay, QuarterLastDay)]
allQuartersBoundaries y =
   map (\q -> (periodFirstDay $ getQuarter q, periodLastDay $ getQuarter q)) [Q1 .. Q4]
   where
       getQuarter q = YearQuarter y q
```

- Get length of the quarters of a given year. Why do we need the year? Because in a
leap year the length of the second quarter will be different:

```haskell
type QuarterLength = Int

getAllQuarterLength :: Year -> [QuarterLength]
getAllQuarterLength y = map (\q -> periodLength $ YearQuarter y q) [Q1,Q2,Q3,Q4]
```

- Get all days of a specific quarter.

```haskell
allDaysQuarter :: Year -> QuarterOfYear -> [Days]
allDaysQuarter y q = periodAllDays $ YearQuarter y q
```

- Get starting and ending days of All months of a given year:

```haskell
type MonthFirstDay = Day
type MonthLastDay  = Day

getAllMonthsBoundaries :: Year -> [(MonthFirstDay, MonthLastDay)]
getAllMonthsBoundaries y =
   map (\m -> (periodFirstDay $ YearMonth y m, periodLastDay $ YearMonth y m))  [January .. December]
```

- Get length of months in a given year:

```haskell
type MonthLength = Int

allMonthsLenght :: Year -> [MonthLength]
allMonthsLenght y = map (\m -> periodLength $ YearMonth y m) [January .. December]
```

- Get all days of a given month:


```haskell
allDaysMonth :: Year -> MonthOfYear -> [Day]
allDaysMonth y m = periodAllDays $ YearMonth y m
```

## Second Contribution

Once the first contribution was merged and that the maintainer performed
all the refactoring, we were able to open a follow up PR for adding some
tests. As I mentioned previously the maintainer added a couple of extra
functions, these are: `dayPeriod`, `periodAllDays` and `periodLength` but
they were not tested. We decided to implement the tests by using patterns
synonyms (yes, once again) and [property testing][property-testing], so
in this section we will review this code. Here is the [PR][PR-2]
that we proposed:

In this context, patterns were pretty useful too. We added a lot of value
almost free. Let me show a piece of code:

```haskell
newtype WDay = MkWDay Day
    deriving (Eq, Show)

instance Arbitrary WDay where
    arbitrary = do
        (MkWYear y) <- arbitrary
        (MkWMonthOfYear m) <- arbitrary
        (MkWDayOfMonth d) <- arbitrary
        pure $ MkWDay $ YearMonthDay y m d
```

This first part is not related to patterns, instead we're declaring the
`Arbitray` instance of `WDay` that is a `Day` wrapper so we can generate
random days in order to make our tests and therefore our code base more robust.
When using property testing we can stop thinking of generating the data,
and we can focus on testing that our code is behaving the way it's supposed
too. We declared wrappers and their `Arbitrary` instaces for: `Day`, `DayOfMonth`,
`Month`, `MonthOfYear`, `Quarter`, `QuarterOfYear` and `Year`. Probably
a better example is the `MonthOfYear` (sounds familiar? Yes, it's the pattern
that we took as an example in the previous section) wrapper and instance, so we
all are clear of what is going on here.

```haskell
newtype WMonthOfYear = MkWMonthOfYear MonthOfYear
    deriving (Eq, Show)
instance Arbitrary WMonthOfYear where
    arbitrary = fmap MkWMonthOfYear $ choose (-5, 17)
```

Why it has the `-5` and `17` boundaries, well this function should be in the
habilty to clip those values to the correct one, so we're testing this too.

I think we can forward, so now we're gonna explore the use of patterns in the
context of test.

```haskell
testMonth :: [TestTree]
	testMonth =
      [ testProperty "periodFirstDay" $ \(MkWMonth my@(YearMonth y m)) ->
        periodFirstDay my == YearMonthDay y m 1
      , testGroup
         "periodLastDay"
         [ testCase "leap year" $
            periodLastDay (YearMonth 2024 February) @?= YearMonthDay 2024 February 29
        , testCase "regular year" $
            periodLastDay (YearMonth 2023 February) @?= YearMonthDay 2023 February 28
         ]
    , testProperty "dayPeriod" $ \(MkWMonth my@(YearMonth y m), MkWDayOfMonth d) ->
        dayPeriod (YearMonthDay y m d) == my
    , testProperty "periodAllDays" $ \(MkWMonth my@(YearMonth y1 m1)) ->
        all (== (y1, m1)) $ map (\(YearMonthDay y2 m2 _) -> (y2, m2)) $ periodAllDays my
    , testGroup
        "periodLength"
        [ testProperty "property tests" $ \(MkWMonth my) ->
            periodLength my >= 28
        , testCase "leap year" $
            periodLength (YearMonth 2024 February) @?= 29
        , testCase "regular year" $
            periodLength (YearMonth 2023 February) @?= 28
        ]
    ]
```

So in the first line we're saying that this test is of the type `testProperty`
(or property test) and that we're testing the `periodFirstDay` function, then we
create a `MkWMonth` that is the arbitrary version of `Month` and is the one that
will generate the randomness for us and finally we compare that the first day of
a month is the same when we use the `periodFirstDay` function than when using the
`YearMonthDay` pattern.

The `periodLastDay` tests are a little peculiar, we didn't use property testing
in this case since we wanted to test some particular cases. We tested that February's
last day in a leap and in a regular is calculated properly.

Well I think the two previous examples are very straight forward, but let me explain
the `periodAllDays` tests. When you're implementing property tests is a little
harder to think of what to test, you have to search a common property (that's why
they are called property test) that let you describe the expected behavior. So what
we're checking in this test is that `all` elements (`Days`) that the `periodAllDays`
function produces, when the `DayPeriod` is a `Month`, belong to the same year and month.
It makes sense right?. Also we can see the best of patterns in action, we're using them
to generate a random month: `MkWMonth my@(YearMonth y1 m1)`, and we're using them to
deconstruct a `Day` into `Year`, `MonthOfYear` and `DayOfMonth`:
`(YearMonthDay y2 m2 _) -> (y2, m2)`.

## Summary

It was a journey, but hopefully we got something from this tutorial. If I have to
highlight something is the following:

- When working on OSS always start by opening an issue and brainstorm your ideas with
the library's mainterners.
- Pattern synonyms is a Haskell feature that will make our code base more reable, and
as shown in this tutorial could be useful in many scenarios.
- Time library now has some cool helper functions that are pretty simple to use and
hopefully pretty useful too.


## Caveat

As you can see we didn't talk about weeks in this tutorial. That's becasue weeks are
not canonical, some years have 52 weeks and others have 53 and depending on what part
of the world you're in, the week could start in different days (Saturday, Sunday,
Monday). We're dicusing this with the maintainer right now but haven't get an answer yet.
But if you want to get all `Days` of a week and specify its starting day, this
[issue][issue-weeks] could be useful.


[issue]: https://github.com/haskell/time/issues/179
[month-pattern]: https://github.com/haskell/time/blob/0b08a17c95ab2b7ad0a667b1dd816d55fa6b4e20/lib/Data/Time/Calendar/Types.hs#L26
[complete-pragma]: https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
[PR-1]: https://github.com/haskell/time/pull/180/files
[PR-2]: https://github.com/haskell/time/pull/182
[maintainer-PR]: https://github.com/haskell/time/commit/0f96de3ea411df8470da37c25be3717bfcd6f098
[property-testing]: https://hackage.haskell.org/package/QuickCheck
[issue-weeks]: https://github.com/haskell/time/issues/183
