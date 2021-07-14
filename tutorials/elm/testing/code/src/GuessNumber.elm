module GuessNumber exposing (..)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Set as S
import Random as R

-- GuessState

type GuessState = RightGuess Int | Attempts (S.Set Int)

insert : Attempt -> GuessState -> GuessState
insert attempt guessList =
  case (attempt, guessList) of
    (_, RightGuess _) -> guessList
    (Correct int, _) -> RightGuess int
    (Wrong int, Attempts attempts) -> Attempts <| S.insert int attempts

type Attempt = Correct Int | Wrong Int

mkAttempt : Int -> Int -> Attempt
mkAttempt guess attempt =
  attempt |>
    if attempt == guess
    then Correct
    else Wrong

empty : GuessState
empty = Attempts S.empty

isEmpty : GuessState -> Bool
isEmpty st =
  case st of
    RightGuess _ -> False
    Attempts attempts -> S.isEmpty attempts

-- Model & Msg

type alias Model =
  { guess: Int
  , isInvalid : Bool
  , attempt: Maybe Int
  , state: GuessState
  }

type Msg
  = Guess (Maybe Int)
  | SendAttempt Int
  | RndGuess Int

type Eff = NoOP | GenRndAttempt ({ onGenerate: Int -> Msg })

init : flags -> (Model, Eff)
init _ = (
  { guess = 0
  , state = empty
  , isInvalid = False
  , attempt = Nothing
  }, GenRndAttempt { onGenerate = RndGuess })

run : Eff -> Cmd Msg
run eff =
  case eff of
    NoOP -> Cmd.none
    GenRndAttempt ({ onGenerate }) ->
      R.generate onGenerate (R.int 1 100)

-- Update

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

-- View

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

-- Main

main : Program () Model Msg
main =
  Browser.element {
    init = \flags -> init flags |> Tuple.mapSecond run
  , update = \msg model -> update msg model |> Tuple.mapSecond run
  , subscriptions = \_ -> Sub.none
  , view = view
  }
