---
title: Time, patterns and a little more
published: 2021-12-02
tags: haskell, pattern-synonyms, property-testing
libraries: time
language: haskell
author: Felix Miño
author-name: Felix Miño
github-profile: felixminom
description: In this tutorial we’re going to explore the use of pattern synonyms in Haskell, later we’ll give a brief overview to the Stack Builders’ contribution to the `time` Haskell library and finally we’ll review how to use the utilities that were introduced in the contribution.
---

As Stack Builders one of the core values of our company is contributing
to open source (OSS). We believe that we have the power to change
people’s lives (or at least make it easier) by pushing the boundaries
of the software industry and we’re happy to announce that the past month
we were able to contribute to one of Haskell's core libraries,
[time][time-hackage].

We started with the idea: “What if we bring some utilities that Ruby on
Rails (RoR) has to Haskell?”. For instance, “How can I get the range of
days that a month has?” or “How can I get a quarter’s boundaries?” were
some of the questions that came to our mind. In the following example
I will show the RoR code needed to get all days in a month and then Haskell
version to do so:

```ruby
today = Date.today
today.next_month.all_month
=> Wed, 01 Nov 2021..Fri, 30 Nov 2021
```

```haskell
allMonth :: Day -> [Day]
allMonth day =
    let (y, m , _) = toGregorian day
    in [fromGregorian y m 1 .. fromGregorian y m 31]
```

and then in `ghci` we can call our function:

```bash
> today <- utctDay <$> getCurrentTime
> monthBoundaries today
[2021-11-01 .. 2021-11-30]
```

As you can see, we had to implement the Haskell code by ourselves. Why are
we setting the last day to always be 31st if some months have less than 31
days? Could be crossing your mind right now. That’s because `fromGregorian`
clips the values to be correct for each month, but now, what is
`fromGregorian`? Well that’s exactly what we wanted to avoid when using
the `time` library, to avoid using some functions that we really don't know
what their purpose is.

We wanted something more clear and readable as Uncle bob mentioned:

>"Indeed, the ratio of time spent reading versus writing is well over
>10 to 1. We are constantly reading old code as part of the effort
>to write new code. ...Therefore, making it easy to read makes it easier
>to write.”

So this was our motivation, to make something similar to the
utilities that RoR has and to ease Haskell developer’s lives.

We started by opening an [issue][issue] on GitHub. Quick parenthesis here,
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

The [GHC documentation][ghc-patterns] describes pattern synonyms like:

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
fortunately, since GHC 7.8 we are able to use pattern synonyms and we
could, instead of using raw `Int`s, write this down in a pattern style
and have a more idiomatic way of defining months:

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
pattern December = 12

{-# COMPLETE January, February, March, April, May, June, July, August, September, October, November, December #-}Year
```

If you’re curious about why the `{-# COMPLETE #-}` pragma is being
used, see the [COMPLETE pragma documentation][complete-pragma] for more
information.

Just to make sure that `November` and `11` are equivalent, let's test it
in a stack repl:

```bash
> November == 11
True
```

We’ll use this pattern in a function that receives a `Day` and yields if the
corresponding Date is a holiday:

```haskell
dayToHoliday :: Day -> String
dayToHoliday (YearMonthDay _ January  _)  = "Happy new year!"
dayToHoliday (YearMonthDay _ November 1)  = "Let's eat colada morada"
dayToHoliday (YearMonthDay _ December 25) = "Merry Christmas jojojo"
dayToHoliday (YearMonthDay _ _ _)         = "Probably just a regular day"
```

Another pattern synonym is introduced here. The `YearMonthDay` pattern lets us
build a `Day` in terms of the `Year`, `MonthOfYear` (do you remember this one?)
and `DayOfMOnth`, where all three are basically `Int`s. Here is its definition:

```haskell
pattern YearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern YearMonthDay y m d <-
   (toGregorian -> (y, m, d))
   where
       YearMonthDay y m d = fromGregorian y m d
```

Back to our function, let's check that is working:

```haskell
> dayToHoliday (YearMonthDay 2021 11 01)
"Let's eat colada morada"

> dayToHoliday (YearMonthDay 2021 November 01)
"Let's eat colada morada"
```

(Now you're probably wondering what is “colada morada”? Haha no worries,
it’s an [Ecuatorian tradition][colada-morada] and we eat this meal that
is purple corn based with spices and fruits, pretty amazing to be honest.)

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
expressions as well, so something like this is totally valid:

``` bash
ghci> dateToHoliday (YearMonthDay 2021 January 12)
"Happy new year!"

ghci> dateToHoliday (YearMonthDay 2021 November 1)
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
Also, some extra features were added, now we can get the days that belong
to a period (`periodAllDays`) and the period's length (`periodLength`).

First of all we started defining the new type class `DayPeriod`:

```haskell
class Ord p => DayPeriod p where
   periodFirstDay :: p -> Day
   periodLastDay :: p -> Day
   dayPeriod :: Day -> p
```

As we can see the `DayPeriod` type class defines three methods: `periodFirstDay`
that will return the first day of a period, `periodLastDay` will return the last
day and `dayPeriod` will return the period the `Day` pass as an argument is in.

One more thing to highlight is that these functions should satisfy some laws
(where `p` is period and `d` is day):

- For all p. `periodFirstDay p <= periodLastDay p`
- For all p. `dayPeriod (periodFirstDay p) == p`
- For all p. `dayPeriod (periodLastDay p) == p`
- For all d. `periodFirstDay (dayPeriod d) <= d`
- For all d. `periodLastDay (dayPeriod d) >= d`

Other particular laws for the `periodLength` function are:

- For `Day`. for all p. `periodLength p == 1`
- For `Month`s. for all p. `periodLength p >= 28`
- For `Quarter`s. for all p. `periodLength p >= 90`
- For `Year`s. for all p. `periodLength p >= 365`

Another particular law for the `periodAllDays` function is:

- For all p. `(periodAllDay) p ∈ p`

The last two sets of laws will be specially usefull later when talking
about property testing.

So after this one was defined all we had to do was create the instances
of the type class for the different data types: `Year`, `Quarter`, `Month`
and `Day` itself, so here it’s the code:

```haskell
instance DayPeriod Year where
   periodFirstDay y = YearMonthDay y January 1
   periodLastDay y = YearMonthDay y December 31
   dayPeriod (YearMonthDay y _ _) = y
```

We're using `YearMonthDay` pattern again and defining that January 1st and
December 31st are the the first and last days of a year. Also we can calculate
the year that a day is in with the `dayPeriod` function.

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

### Examples of some new `time` features

So let's see what we can do with these new features introduced in the contribution.
In this section we will review some concrete code samples of how these new features
can help us in our daily software development routine. After making the proper imports
we can have something like this:

- Let's compare the first function that I presented, how to get a month's all days.
Now in `ghci` we can directly call the `periodAllDays` function with a `Month` and
that will be enough:

    ```bash
    ghci> periodAlldays  (YearMonth 2021 November)
    [2021-11-01 .. 2021-11-30
    ```

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

- Get all days of a specific quarter:

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
to. We declared wrappers and their `Arbitrary` instaces for: `Day`, `DayOfMonth`,
`Month`, `MonthOfYear`, `Quarter`, `QuarterOfYear` and `Year`. Probably
a better example is the `MonthOfYear` (sounds familiar? Yes, it's the pattern
that we took as an example in the previous section) wrapper and instance, so we
all are clear of what is going on here:

```haskell
newtype WMonthOfYear = MkWMonthOfYear MonthOfYear
    deriving (Eq, Show)
instance Arbitrary WMonthOfYear where
    arbitrary = fmap MkWMonthOfYear $ choose (-5, 17)
```

Why does it have `-5` and `17` boundaries? Well, this function should be able
to clip those values to the correct ones, so we're testing this too.

I think we can move forward, so now we're going to explore the use of patterns
in the context of tests:

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

All of these test are written and will be explained in the `Month`s context.
So in the first line we're saying that this test is of the type `testProperty`
(or property test) and that we're testing the `periodFirstDay` function, then we
create a `MkWMonth` that is the arbitrary version of `Month` and is the one that
will generate the randomness for us and finally we compare that the first day of
a month is the same when we use the `periodFirstDay` function than when using the
`YearMonthDay` pattern.

The `periodLastDay` tests are a little peculiar, we didn't use property testing
in this case since we wanted to test some particular cases. We tested that
February's last day calculated properly for both leap and regular years. As you
can see in some case unit tests can be useful too, so don't stick too much to
property testing.

Well I think the two previous examples are very straightforward, but let me
explain the `periodAllDays` tests. When you're implementing property tests is a
little harder to think of what to test, you have to search a common property
(that's why they are called property test) that let you describe the expected
behavior. So what we're checking in this test is that `all` elements (`Days`)
that the `periodAllDays` function produces, when the `DayPeriod` is a `Month`,
belong to the same year and month.

Finally the `periodLength` was tested and in this case the test suite is a
combination of unit tests and property tests. We are checking that
the period's lenght of all months is at least 28 days and that February's
lenght is correctly calculated in a leap and in a regular year.

Do you remember the laws that we mentioned in the previous section, well those
were translated to property tests. Isn't it awesome? When we work and create
property tests we need to think out of the box and find the property (law) that
every test subject should satisfy, but once that is found is pretty clear what
we need to check for.

It makes sense right?. Also we can see the best of patterns in action, we're
using them to generate a random month: `MkWMonth my@(YearMonth y1 m1)`, and
we're using them to deconstruct a `Day` into `Year`, `MonthOfYear` and
`DayOfMonth`: `(YearMonthDay y2 m2 _) -> (y2, m2)`.

## Summary

It was a journey, but hopefully we got something from this tutorial. If I have to
highlight something is the following:

- When working on OSS always start by opening an issue and brainstorm your ideas with
the library's mainterners.
- Pattern synonyms is a Haskell feature that will make our code base more reable, and
as shown in this tutorial could be useful in many scenarios. The `YearMonthDay` pattern
is probably one of the more versatile ones, so if you're working with the `time`
library have this in mind.
- Time library now has some cool helper functions that are pretty simple to use
and hopefully pretty useful too. Keep in mind the `periodFirstDay`, `periodLastDay`,
`periodAllDays` and `periodLength` functions.

## Caveat

As you can see we didn't talk about weeks in this tutorial. That's becasue weeks are
not canonical, some years have 52 weeks and others have 53 and depending on what part
of the world you're in, the week could start in different days (Saturday, Sunday,
Monday). We're dicussing this with the maintainer right now but haven't gotten an answer yet.
But if you want to get all `Days` of a week and specify its starting day, this
[issue][issue-weeks] could be useful.


[time-hackage]: https://hackage.haskell.org/package/time
[issue]: https://github.com/haskell/time/issues/179
[month-pattern]: https://github.com/haskell/time/blob/0b08a17c95ab2b7ad0a667b1dd816d55fa6b4e20/lib/Data/Time/Calendar/Types.hs#L26
[complete-pragma]: https://gitlab.haskell.org/ghc/ghc/-/wikis/pattern-synonyms/complete-sigs
[PR-1]: https://github.com/haskell/time/pull/180/files
[PR-2]: https://github.com/haskell/time/pull/182
[maintainer-PR]: https://github.com/haskell/time/commit/0f96de3ea411df8470da37c25be3717bfcd6f098
[property-testing]: https://hackage.haskell.org/package/QuickCheck
[issue-weeks]: https://github.com/haskell/time/issues/183
[colada-morada]: https://en.wikipedia.org/wiki/Colada_morada
[ghc-patterns]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/pattern_synonyms.html
