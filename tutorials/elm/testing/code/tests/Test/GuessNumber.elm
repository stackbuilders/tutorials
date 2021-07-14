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


guessStateTests : Test
guessStateTests =
  describe "insert"
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
            |> E.equal { model | isInvalid = True }
      ]
  ]

viewTests : Test
viewTests =
  describe "view"
  [
    describe "when isInvalid is False" [
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

tests : Test
tests =
  describe "GuessNumber"
  [
      test "shows initial message" <|
        \() ->
          start
            |> PT.expectViewHas [ text "from 1 to 100" ],
      test "shows waiting message" <|
        \() ->
          start
            |> PT.expectViewHas [ text "Has not tried yet" ],
      test "given invalid input shows message" <|
        \() ->
          start
          |> PT.fillIn "number" "Number" "boo"
          |> PT.clickButton "Guess!"
          |> PT.expectViewHas
            [ text "Invalid" ],
      test "shows list of wrong attempts" <|
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
            ],
      test "shows message when guess is correct" <|
        \() ->
          start
            |> PT.simulateLastEffect (\_ -> Ok [GN.RndGuess 10])
            |> PT.fillIn "number" "Number" "10"
            |> PT.clickButton "Guess!"
            |> PT.expectViewHas [
              text "You guessed 10 correctly"
            ]
  ]
