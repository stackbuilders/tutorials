---
title: Win the test trophy in Elm!
published: 2021-07-14
tags: elm, testing
libraries: elm-0.19
language: elm
author-name: Cristhian Motoche
twitter-profile: Camm_V222
github-profile: CristhianMotoche
description: In this tutorial, we'll review how to test an Elm application following the test trophy
---

Software applications [must work and should reach a certain level of confidence][test-confidence] that allows software developers to change code without worrying too much about breaking it. We can use Elm along with a testing strategy to get a high level of confidence and avoid regressions. The [test trophy strategy][test-trophy-shape] suggests that we should write tests but not too many, mainly integration. In this tutorial, we'll review the test trophy applied in an Elm application.

# What is the test trophy?
The test trophy is a test strategy, like the [test pyramid][test-pyramind], that suggests what tests we should write and in which quantity. The [test trophy groups tests][test-trophy-layers] into the following layers:

1. Static analysis
1. Unit tests
1. Integration tests
1. End-to-End tests*

This strategy is focused on integration tests that are quick to write and execute thanks to new and modern tools. Integration tests give a higher level of confidence because they test multiple software components at the same time.

**\*Note:** We won't cover end-to-end tests in this tutorial to keep it short, and because it deserves its own separate tutorial.

# Why Elm?
[Elm][elm-guide] is a functional and type safe programming language that compiles to JavaScript. It guarantees no [runtime errors][no-runtime-errors], friendly error messages, and reliable refactoring. The test trophy suggests that we should have static checks as a base for our application. Therefore, I consider Elm to be a great base for this strategy.

## Elm architecture
In an Elm application, we follow the [Model View Update][elm-mvu] pattern. In this pattern, we define a `Model` to represent the state of our program, a `view` function to represent the state of our model as HTML, and an `update` function which changes the state of our program.

# How to test an Elm application following the test trophy strategy?
## Set it up
To begin with, let's set up our environment. First, we need to create an Elm and Node project:

```
elm init
npm init -y
```

Next, we need to add `elm-test` to our dependencies:

```
npm install elm-test --save-dev
```

In addition, we'll edit `package.json` to execute `elm-test` from the `test` script in the npm scripts:
```json
  "scripts": {
    "test": "elm-test"
  }
```

Now, we need to start our test suite:

```
npm test init
```

The previous command will add the [`elm-expectations/test`][elm-expectations-test] package to our testing dependencies and it will create a directory called `tests` in our working directory.

After that, we'll add a couple of packages for our application and tests: `elm/random` and `avh4/elm-program-test`:

```
elm install avh4/elm-program-test
elm install elm/random
```

`elm/random` and `avh4/elm-program-test` will be used for our program and our integration tests, respectively.

## Subject Under Test

For this tutorial, the subject under test is a simple guessing number program. In this program, a number between 1 and 100 is randomly generated when the application is started. The user submits a number trying to guess the generated number. An incorrect attempt is added to a list, and a correct attempt finishes the program. Let's define this program:

1) Define a module and the necessary imports:

```haskell
module GuessNumber exposing (..)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Set as S
import Random as R
```

2) Define the model (`Model`) and the types of messages (`Msg`) for the program:

```haskell
type alias Model =
  { guess: Int -- Random number to be guessed
  , isInvalid : Bool -- Simple flag to check if the input is a number or not
  , attempt: Maybe Int -- Possible attempt from the user input
  , state: GuessState  -- State of the guess game
  }

type Msg
  = Guess (Maybe Int)  -- Holds the possible attempt
  | SendAttempt Int  -- Holds the numeric attempt to be checked
  | RndGuess Int  -- Holds the random number
```

3) Define the `view` function:

```haskell
view : Model -> H.Html Msg
view model =
  H.div []
  [
    viewForm model, viewGuesses model.state
  ]

viewForm : Model -> H.Html Msg
viewForm model =
  H.div
    []
    [
      H.p [] [ H.text "Guess a number from 1 to 100" ],
      H.p
        []
        [
          if model.isInvalid
          then H.text "Invalid input"
          else H.text ""
        ],
      H.form
        [
          HE.onSubmit (
            Maybe.withDefault (Guess Nothing)
              <| Maybe.map SendAttempt model.attempt
          )
        ]
        [
          H.input
            [ HA.placeholder "Guess a number",
              HA.type_ "number",
              HA.id "number",
              HA.attribute  "aria-label" "Number",
              HE.onInput (Guess << String.toInt)
            ]
            [],
          H.input
            [ HA.type_ "submit",
              HA.value "Guess!"
            ]
            []
        ]
    ]


viewGuesses : GuessState -> H.Html Msg
viewGuesses state =
  H.div []
  [
    if isEmpty state
    then H.text "Has not tried yet"
    else
      case state of
        RightGuess correct ->
          H.text <| "You guessed " ++ String.fromInt correct ++ " correctly!"
        Attempts attempts ->
          H.ul [] <| List.map viewGuess <| S.toList attempts
  ]

viewGuess : Int -> H.Html Msg
viewGuess int = H.li [][ H.text <| "Already tried " ++ String.fromInt int ]
```

4) Define the `update` function, which updates the state of the program based on the messages:

```haskell
update : Msg -> Model -> (Model, Eff)
update msg model =
  case msg of
    Guess Nothing -> ({ model | isInvalid = True }, NoOP)
    Guess value -> ({ model | attempt = value, isInvalid = False }, NoOP)
    SendAttempt value ->
      let
          attempt = mkAttempt model.guess value
      in
        ({
          model |
              state = insert attempt model.state, isInvalid = False
        }, NoOP)
    RndGuess guess -> ({ model | guess = guess }, NoOP)
```

5) Define `init` which represents the initial state of the program:

```haskell
init : flags -> (Model, Eff)
init _ = (
  { guess = 0
  , state = empty
  , isInvalid = False
  , attempt = Nothing
  }, GenRndAttempt { onGenerate = RndGuess })
```

If you've tried Elm before, you might be wondering about `Eff`. Usually the `update` function returns `Model` or `(Model, Cmd Msg)`.  In this case, `Eff` is just a type that represents the effects in our program. It looks like this:

```haskell
type Eff = NoOP | GenRndGuess ({ onGenerate: Int -> Msg })
```

Currently, the details of `Cmd msg` are not available. So, `Eff` is necessary to simulate effects in our integration tests, which we'll review later in this post.

Finally, define `main` which plugs everything together. Of course, `main` needs `update` to match the type `Msg -> Model -> (Model Cmd Msg)` and `init` to match `flags -> (Model, Cmd Msg)`. Consequently, we need a way to convert `Eff` back into `Cmd Msg`. The function `run` will do that:


```haskell
run : Eff -> Cmd Msg
run eff =
  case eff of
    NoOP -> Cmd.none
    GenRndGuess ({ onGenerate }) ->
      R.generate onGenerate (R.int 1 100)
```

Then, we can define `main` like this:

```haskell
main : Program () Model Msg
main =
  Browser.element {
    init = \flags -> init flags |> Tuple.mapSecond run
  , update = \msg model -> update msg model |> Tuple.mapSecond run
  , subscriptions = \_ -> Sub.none
  , view = view
  }
```

**\*Note:** `GuessState` and `mkAttempt` are defined in the next section.

## Static check
Elm is a functional and type safe programming language. Additionally, it avoids runtime errors and the compiler gives useful error messages to do fearless refactoring. Also, we can [make impossible states impossible][misi], so that we avoid writing some tests! Therefore, it covers the static analysis section very well in the test trophy.

### Our data types

`GuessState` will hold our attempts or the correct number guessed, and `Attempt` will wrap a correct or wrong attempt:

```haskell
type GuessState = RightGuess Int | Attempts (S.Set Int)

type Attempt = Correct Int | Wrong Int
```

The smart constructor `mkAttempt` can be used to insert attempts into the `GuessState` data structure:

```haskell
mkAttempt : Int -> Int -> Attempt
mkAttempt guess attempt =
  attempt |>
    if attempt == guess
    then Correct
    else Wrong

insert : Attempt -> GuessState -> GuessState
insert attempt guessList =
  case (attempt, guessList) of
    (_, RightGuess _) -> guessList
    (Correct int, _) -> RightGuess int
    (Wrong int, Attempts attempts) -> Attempts <| S.insert int attempts
```

`GuessState` can only have one right guess and a unique set of attempts. Therefore, we don't need to test if the list of attempts has repeated values, or if there are more than two correct attempts. Those cases won't happen in this data structure!

## Unit tests

We can write expectations and fuzzy tests to unit test our code. First, let's define a testing module with the following definition and imports:

```haskell
module Test.GuessNumber exposing (..)

import GuessNumber as GN
import Test exposing (..)
import Test.Html.Selector exposing (text)
import Test.Html.Query as Query
import Fuzz as F
import Expect as E
import Set as S
import Tuple

import ProgramTest as PT
import SimulatedEffect.Cmd as SCmd
import SimulatedEffect.Task as STask
```

### Simple Expectations
We can cover unit test cases using the [expectation module][Expect]. For example:

1. `insert` returns a `RightGuess` when the input is `Correct`.
1. `insert` appends `Wrong` attempts to the set of `Attempts`.

```haskell
  describe "insert
  [
    describe "given a correct attempt"
    [
      test "produces a correct GuessState" <|
        \_ ->
          GN.insert (GN.Correct 10) GN.empty
          |> E.equal (GN.RightGuess 10)
    ],
    describe "given a wrong attempt"
    [
      test "produces a new attempt in the GuessState" <|
        \_ ->
          GN.insert (GN.Wrong 10) GN.empty
          |> E.equal (GN.Attempts (S.insert 10 S.empty))
    ]
  ]
```

These tests are simple in that they only cover specific cases of what `insert` does, but they can help as examples and help document the purpose of this function.

### Fuzzy tests
[Property tests or fuzzy tests][property-tests] will increase our confidence in our code because they test multiple scenarios to make sure that a property is satisfied. For example, test that `insert` leaves the game state the same after a correct attempt, it does not matter which is the following attempt. We can write this property test using the [Fuzz module][Fuzz].

```haskell
fuzzTest : Test
fuzzTest =
  describe "props insert"
  [
    fuzz (F.tuple (F.int, F.int)) "only one right guess" <|
      \(rndA, rndB) ->
         GN.insert (GN.mkAttempt 0 0) GN.empty
           |> GN.insert (GN.mkAttempt rndA rndB)
           |> E.equal (GN.RightGuess 0)
  ]
```

There is much more to say about [property tests][elm-fuzz-testing], but that could be part of a separated post.

### Testing `update` and `view`?

We could add some unit tests for `update` and `view`:

```haskell
updateTests : Test
updateTests =
  describe "update"
  [
    describe "given Guess Nothing" [
      test "updates isInvalid to true" <|
        \_ ->
          let
              model = Tuple.first <| GN.init ()
          in
            GN.update (GN.Guess Nothing) model
            |> Tuple.first
            |> E.equal { model | isInvalid = False }
      ]
  ]

viewTests : Test
viewTests =
  describe "view"
  [
    describe "when isInvalid is True" [
      test "Shows invalid input message" <|
        \_ ->
          let
              model = Tuple.first <| GN.init ()
          in
            GN.view ({ model | isInvalid = True })
            |> Query.fromHtml
            |> Query.has [ text "Invalid input" ]
      ]
  ]
```

However, we can have multiple initial states and messages, so testing every possible state with every message will produce a very long test suite. Additionally, if we rename a model attribute or a message, we'll have to update it in our tests, so our code will be very tight with our tests. For example, try to change the logic of the attribute `isInvalid` to `isValid`. You'll see how these tests stop compiling. I'd recommend avoiding these tests, but you can write them if you find them helpful.

## Integration tests
The test trophy suggests that we should focus on adding integration tests. `avh4/elm-program-test` comes handy for these tests.

First, let's identify some test cases. I came up with the following:

1. Given an incorrect input value (e.g. an alphabetic character), then it notifies the user about an invalid input.
1. Given some wrong attempts, then all attempts are listed.
1. Given a correct attempt, then it notifies the user about a correct attempt.

Ok, let's write them!

### Setting up effects

I mentioned earlier that `Cmd` details are not reveled, so we had to create an intermediate type (`Eff`) that can help us to simulate the effects in our test module. We already defined the effects for our program in `run`, now let's do the same for our tests.

```haskell
perform : msg -> PT.SimulatedEffect msg
perform = STask.perform identity << STask.succeed

simulateEffs : Int -> GN.Eff -> PT.SimulatedEffect GN.Msg
simulateEffs guess eff =
  case eff of
    GN.NoOP -> SCmd.none
    GN.GenRndAttempt { onGenerate } ->
      perform <| onGenerate guess

start : PT.ProgramTest GN.Model GN.Msg GN.Eff
start =
    PT.createElement {
        init = GN.init
      , update = GN.update
      , view = GN.view
    }
    |> PT.withSimulatedEffects (simulateEffs 0)
    |> PT.start ()
```

`perform` puts some input into `SimulatedEffect`, similar to Haskell `pure` function. `simulateEffs` defines how our effects are simulated and takes a number to simulate the generation of a random value*. Finally, `start` takes `init`, `update`, `view` and flags (none in this example) to start the program for testing.

**\*Note:** There is an [open issue][random-issue] to simulate the effects of random generated numbers. That will make things easier in the future. I'll update this post once the issue is solved.

### Writing our tests

Let's implement the first test case following these steps:

1. Look for the `"Number"` input field,
1. Insert the input text `"boo"`,
1. Click on the button `"Guess!"`, and
1. Expect to see the message `"Invalid"`.

```haskell
      test "with an invalid input shows message" <|
        \() ->
          start
          |> PT.fillIn "number" "Number" "boo"
          |> PT.clickButton "Guess!"
          |> PT.expectViewHas
            [ text "Invalid" ]
```
**Note:** Currently, `fillIn` takes two arguments. The first one is the `id` of the element. It will be removed once this [issue][id-issue] is resolved, though.

Now, the second test case:

1. Look for the `"Number"` input field,
1. Make some wrong attempts, and
1. Expect to see the wrong attempts listed.

```haskell
      test "shows list of wrong messages" <|
        \() ->
          start
            |> PT.fillIn "number" "Number" "1"
            |> PT.clickButton "Guess!"
            |> PT.fillIn "number" "Number" "2"
            |> PT.clickButton "Guess!"
            |> PT.fillIn "number" "Number" "3"
            |> PT.clickButton "Guess!"
            |> PT.expectViewHas [
              text "Already tried 1"
            , text "Already tried 2"
            , text "Already tried 3"
            ]
```

We used `text` from the [Selector module][Selector] in this test too, but `expectViewHas` can take any expectation from the module.

Finally, our last test case follows these steps:

1. Simulate the random generation with the value `10`,
1. Look for the `"Number"` input field,
1. Insert the input text `"10"`, and
1. Expect to see the message: `"You guessed 10 correctly"`.

```haskell
      test "shows message when guess is correct" <|
        \() ->
          start
            |> PT.simulateLastEffect (\_ -> Ok [GN.RndGuess 10])
            |> PT.fillIn "number" "Number" "10"
            |> PT.clickButton "Guess!"
            |> PT.expectViewHas [
              text "You guessed 10 correctly"
            ]
```
In this case, we used `simulateLastEffect` to simulate a different random number for this particular test case.

These integration tests provide the following benefits:

1. Tests are not tied to implementation details. We can rename an attribute in our Model or a Message and we won't have to update our tests.
1. They test `view` and `update` together simulating actions that could occur in a real application.
1. Tests are easy to understand and change.
1. They add more confidence to our code base.
1. We can create simple helpers to reduce the tests even more. After all, the actions are composable. For example:

```haskell
clickWithInput input =  PT.fillIn "number" "Number" input |> PT.clickButton "Guess!"
```

I just showed the tip of the iceberg of `avh4/elm-program-test`. It can also be used to simulate HTTP calls, subscription events, and much more.

Do you want to give it a try? You can find the code for this tutorial [here][code-repo]. Try adding new functionality and integration tests. For example: limiting the wrong attempts to 5, or allowing users to restart the game. I'd love to see what you came up with. Happy hacking!

# Conclusion
The modern tools available for Elm applications let us write tests following the tests trophy shape. The Elm compiler covers very well the static checks and we won't have to write some tests if we make impossible states. Also, we can use `elm-test` to write unit tests and fuzzy tests which cover many test cases. Finally, `elm-program-test` is the key to increase our confidence level because it helps us to write integration tests in a delightful way.

[test-confidence]: https://stackoverflow.com/questions/153234/how-deep-are-your-unit-tests/153565#153565
[no-runtime-errors]: https://guide.elm-lang.org/error_handling/
[test-trophy-shape]: https://kentcdodds.com/blog/write-tests
[misi]: https://www.youtube.com/watch?v=IcgmSRJHu_8
[test-pyramind]: https://martinfowler.com/bliki/TestPyramid.html
[elm-lang]: https://elm-lang.org/
[elm-mvu]: https://guide.elm-lang.org/architecture/
[elm-expectations-test]: https://package.elm-lang.org/packages/elm-explorations/test/1.2.2/
[Expect]: https://package.elm-lang.org/packages/elm-explorations/test/latest/Expect
[Fuzz]: https://package.elm-lang.org/packages/elm-explorations/test/latest/Fuzz
[Selector]: https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Selector
[property-tests]: https://en.wikipedia.org/wiki/Property_testing
[elm-fuzz-testing]: https://elmprogramming.com/fuzz-testing.html
[random-issue]: https://github.com/avh4/elm-program-test/issues/52
[id-issue]: https://github.com/eeue56/elm-html-test/issues/52
[elm-guide]: https://guide.elm-lang.org/
[test-trophy-layers]: https://testingjavascript.com/
[code-repo]: https://github.com/stackbuilders/tutorials/blob/master/tutorials/elm/testing/code/
