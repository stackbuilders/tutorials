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

As Stack Builders one of the core values of our company is contributing to open
source (OSS). We believe that we have the power to change people’s lives (or at
least make it easier) by pushing the boundaries of the software industry and
we’re happy to announce that we were able to contribute to one of Haskell's core
libraries, [time][time-hackage].

All started with the idea: “What if we bring some utilities that Ruby on Rails
(RoR) has to Haskell?”. For instance, “How can one get the range of days that a
month has?” or “How can one get a quarter’s boundaries?” were some of the
questions that came to our mind. In the following example the RoR code needed to
get all days in a month will be shown and then Haskell version to do so:

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

and then in `ghci` this function can be invoked:

```bash
> today <- utctDay <$> getCurrentTime
> allMonth today
[2021-11-01 .. 2021-11-30]
```

As you can see, the Haskell code has to be implemented to then be called, there
is not a helper function that performs the same behavior as in RoR. Why the last
day of the month is always set to be 31st, if some months have less than 31 days?
Could be crossing your mind right now. That’s because `fromGregorian` clips the
values to be correct for each month, but now, what is `fromGregorian`? Well
that’s exactly what this contribution aims, adding some alternative helper
functions with clearer names when using the `time` library.

As Uncle bob mentioned:

>"Indeed, the ratio of time spent reading versus writing is well over 10 to 1.
>We are constantly reading old code as part of the effort to write new code.
>...Therefore, making it easy to read makes it easier to write.”

So this was the motivation, to make something similar to the utilities that RoR
has and to ease Haskell developers' lives.

So first of all, an [issue][issue] on GitHub was opened. Quick parenthesis here,
when you want to work on OSS it's recommended to start by opening an issue in
the repo and discuss/brainstorm your ideas with the maintainers, sometimes there
are some good reasons why the feature you’re proposing have not been yet
implemented and if you open a PR directly it could rejected, so save yourself a
disappointment and open an issue first. Back to the issue, the `time`'s library
maintainer proposed to implement these helper functions using pattern synonyms,
so let's start with a quick introduction to some basics about this Haskell
feature that, actually, it has been around for some years now.

## A little about pattern synonyms

First of a little caveat, this guide is not meant to explain pattern synonyms in
depth, instead it’s meant to be a practical introduction to this concept.

The [GHC documentation][ghc-patterns] describes pattern synonyms like:

> “Pattern synonyms enable giving names to parametrized pattern schemes.
> They can also be thought of as abstract constructors that don’t have a
> bearing on data representation.”

And probably this does not make sense yet, so let's introduce pattern synonyms
with a real world example. Let's borrow a pattern that exists in the time library.
The [MonthOfYear][month-pattern] pattern allows to define months of the year
not by a number but by its name.

The `MonthOfYear` data type is defined like this:

```haskell
type MonthOfYear = Int
```

As you can see, `MonthOfYear` is only an alias for `Int`. Now, it's possible to
define months like `Int`s, where January will correspond to 1 and so on. But
fortunately, since GHC 7.8 it's feasible to use pattern synonyms and, instead of
using raw `Int`s is possible to write this down in a pattern style and have a
more idiomatic way of defining months:

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
in a repl:

```bash
> November == 11
True
```

To exemplify, this pattern will be used in a function that receives a `Day` and
yields if the corresponding aate is a holiday:

```haskell
dayToHoliday :: Day -> String
dayToHoliday (YearMonthDay _ January  _)  = "Happy new year!"
dayToHoliday (YearMonthDay _ November 1)  = "Let's eat colada morada"
dayToHoliday (YearMonthDay _ December 25) = "HO HO HO Merry Christmas"
dayToHoliday _                            = "Probably just a regular day"
```

Another pattern synonym is introduced here. The `YearMonthDay` pattern builds
a `Day` in terms of the `Year`, `MonthOfYear` (do you remember this one?) and
`DayOfMOnth`, where all three are basically `Int`s. Here is its definition:

```haskell
pattern YearMonthDay :: Year -> MonthOfYear -> DayOfMonth -> Day
pattern YearMonthDay y m d <-
   (toGregorian -> (y, m, d))
   where
       YearMonthDay y m d = fromGregorian y m d
```

Back to the `dayToHoliday` function, let's check that is working:

```haskell
> dayToHoliday (YearMonthDay 2021 11 01)
"Let's eat colada morada"

> dayToHoliday (YearMonthDay 2021 12 25)
"HO HO HO Merry Christmas"
```

(Now you're probably wondering what “colada morada” is? Haha no worries, it’s an
[Ecuatorian meal][colada-morada] that is purple corn based with spices and
fruits, pretty amazing to be honest.)

Back to Haskell, probably it’s pretty clear what the `dateToHoliday` function
does, the first clause matches all days of January, second one matches the All
Saints holiday, the third one matches for Christmas and the last one it’s the
fallback for all other days. Using the `MonthOfYear` and `YearMontDay` patterns
makes the code more readable. Having something like:

```haskell
dateToHoliday 12 25  = "HO HO HO Merry Christmas"
```

is not as straightforward and readable as the function that was presented in the
previous code sample.

It has not being mentioned yet, but the patterns that were built previously, are
called bidirectional patterns, why? Because one can use them as expressions as
well, so something like this is totally valid:

``` bash
ghci> dateToHoliday (YearMonthDay 2021 January 12)
"Happy new year!"

ghci> dateToHoliday (YearMonthDay 2021 November 1)
"Let's eat colada morada"
```

So now it's possible to use `11`, `November` in the `MonthOfYear` context
interchangeably across the code.

## Stack Builders contribution to Haskell’s time library

Well all of this preamble was needed to present the contribution itself. Pattern
synonyms is what was used when working on `time`’s new features. You can check
the complete [pull request][PR-1](PR). It's important to clarify something, the
PR has some different code than the one that is going to be reviewed here, since
the maintainer decided to do some renaming and changed the original PR’s code,
you can check the changes [here][maintainer-PR]. Also, some extra features were
added, and now it's possible to get the days that belong to a period
(`periodAllDays`) and a period's length (`periodLength`).

The new type class `DayPeriod` was declared as the first step:

```haskell
class Ord p => DayPeriod p where
   periodFirstDay :: p -> Day
   periodLastDay :: p -> Day
   dayPeriod :: Day -> p
```

As you can see the `DayPeriod` type class defines three methods: `periodFirstDay`
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

- For all p. `(periodAllDay p) ∈ p`

The last two sets of laws will be specially usefull later when talking about
property testing, so keep these in mind.

After the `DayPeriod` type class was defined all that is missing is to create
the instances of the type class for the different data types: `Year`, `Quarter`,
`Month` and `Day` itself, so here is the code:

```haskell
instance DayPeriod Year where
   periodFirstDay y = YearMonthDay y January 1
   periodLastDay y = YearMonthDay y December 31
   dayPeriod (YearMonthDay y _ _) = y
```

The `YearMonthDay` pattern is being used again to define that January 1st and
December 31st are the the first and last days of a year, respectively. Also it's
possible to know the year that a day is in with the `dayPeriod` function.

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

This instance is defined in terms of the `periodFirstDay` and `periodLastDay`
of a `Month` and it uses (once again) a pattern named `YearMonth`.


Let’s move over to the `Month` instance of `DayPeriod`:

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

As you could imagine, the first and last day of a day is the day itself and the
period is also the same day.

### Examples of some new `time` features

So let's see what these new features that were introduced in the contribution
offers. This section will present some concrete code samples of how these new
features can help you in your daily software development routine. After making
the proper imports it's possible to have something like this.

- Let's compare the first function that was presented, how to get a month's all
days. Now in `ghci` it's possible to directly call the `periodAllDays` function
with a `Month` and that will be enough:

    ```bash
    ghci> periodAlldays (YearMonth 2021 November)
    [2021-11-01 .. 2021-11-30]
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

- Get quarters' lenght of a given year. Why the year is needed? Because in a
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

- Get starting and ending days of every month of a given year:

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

## Second Contribution

Once the first contribution was merged and that the maintainer performed all the
refactoring, a follow up PR was opened, the purpose was to add some testing. As
it was mentioned previously the maintainer added a couple of extra functions,
these are: `dayPeriod`, `periodAllDays` and `periodLength` but they were not
tested. The tests implemented in this contribution made use of patterns synonyms
(yes, once again) and [property testing][property-testing], so in this section
this code will review . Here is the [PR][PR-2] that was proposed:

In this context, patterns were pretty useful too. A lot of value was added almost
free. Let's review a piece of code:

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

This first part is not related to patterns, instead the `Arbitray` instance of
`WDay` is being declared, this instance is basically a `Day` wrapper that allows
to generate random days in order to perform property testing. When using property
testing you can stop thinking of generating the data that will feed the tests
manually,and you can focus on testing that the code is behaving the way it's
supposed to. The `Arbitrary` instaces for: `Day`, `DayOfMonth`,
`Month`, `MonthOfYear`, `Quarter`, `QuarterOfYear` and `Year` were declared too.
Probably a better example of this instances is the `MonthOfYear` instance (sounds
familiar? Yes, it's the pattern that was shown as an example in the previous
section), so what is going on here is all clear:

```haskell
newtype WMonthOfYear = MkWMonthOfYear MonthOfYear
    deriving (Eq, Show)
instance Arbitrary WMonthOfYear where
    arbitrary = fmap MkWMonthOfYear $ choose (-5, 17)
```

Why does it have `-5` and `17` boundaries? Well, this function should be able
to clip those values to the correct ones, so this clipping is being tested too.

Let's move forward to explore the use of patterns in the context of tests:

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

All of these test are written and will be explained in the `Month`'s context. So
in the first line this test is being declared to be of the type `testProperty`
(or property test) and then it yields the function that is being tested
(`periodFirstDay`), then it creates a `MkWMonth` that is the arbitrary version
of `Month` and is the one that will generate the randomness for property testing
and finally it compares that the first day of a month is the same when the
`periodFirstDay` function is used than when using the `YearMonthDay` pattern.

The `periodLastDay` tests are a little peculiar, property testing was not used
in this case since it was needed to test some particular cases. Here the tests
are checking that February's last day is calculated properly for both leap and
regular years. As you can see in some cases unit tests can be useful too, so
don't stick too much to property testing.

Well, probably the two previous examples are very straightforward, but let's see
what the `periodAllDays` tests are doing. When implementing property tests is a
little harder to think of what to test, you have to search a common property
(that's why they are called property test) that let you describe the expected
behavior. So what it's being checked in this test is that `all` elements (`Days`)
that the `periodAllDays` function produces, when the `DayPeriod` is a `Month`,
belong to the same year and month.

Finally the `periodLength` was tested and in in this case the test suite is a
combination of unit tests and property tests. Test are checking that the period's
lenght of all months is at least 28 days and that February's lenght is correctly
calculated in a leap and in a regular year.

Do you remember the laws that were mentioned in the previous section, well those
were translated to property tests. Isn't it awesome? When working and creating
property tests you need to think out of the box and find the property (law) that
every test subject should satisfy, but once that is found is pretty clear what
you should check for.

In this test code snippet the best of patterns is put in action. They're being
used to generate a random month: `MkWMonth my@(YearMonth y1 m1)`, and to
deconstruct a `Day` into `Year`, `MonthOfYear` and
`DayOfMonth`: `(YearMonthDay y2 m2 _) -> (y2, m2)`.

## Summary

It was a journey, but hopefully you got something from this tutorial. If there
are things that need to be highlighted, they would be the following:

- When working on OSS always start by opening an issue and brainstorm your ideas
with the library's mainterners.

- Pattern synonyms is a Haskell's feature that will make the code base more
reable, and as shown in this tutorial could be useful in many scenarios.
The `YearMonthDay` pattern is probably one of the more versatile ones, so if
you're working with the `time` library have this in mind.

- Time library now has some cool helper functions that are pretty simple to use
and hopefully pretty useful too. Keep in mind the `periodFirstDay`,
`periodLastDay`, `periodAllDays` and `periodLength` functions.

## Caveat

As you can see weeks were not mentioned in this tutorial. That's becasue weeks
are not canonical, some years have 52 weeks and others have 53 and depending on
what part of the world you're in, the week could start in different days
(Saturday, Sunday, Monday). A dicussion with the maintainer is going on right
now but haven't gotten an answer yet. But if you want to get all `Days` of a week
and specify its starting day, this [issue][issue-weeks] could be useful.


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
