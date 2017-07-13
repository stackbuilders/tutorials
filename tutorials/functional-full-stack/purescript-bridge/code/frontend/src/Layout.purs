module App.Layout where

import App.Counter as Counter
import App.Scientist as Scientist
import App.NotFound as NotFound
import App.Routes (Route(Home, NotFound))
import Prelude (($), map)
import Pux.Html (Html, div, h1, p, text)

data Action
  = ScientistAction (Scientist.Action)
  | ScientistsLoaded (Scientist.State)
  | PageView Route
  | Nop

type State =
  { route :: Route
  , sScientists :: Scientist.State }

init :: State
init =
  { route: Home
  , sScientists: Scientist.init }

update :: Action -> State -> State
update (PageView route) state = state { route = route }
update (ScientistAction action) state = state { sScientists = Scientist.update action state.sScientists }
update (ScientistsLoaded s) state = state { sScientists = s }
update Nop state = state

view :: State -> Html Action
view state =
  div
    []
    [ h1 [] [ text "Scientist Viewer" ]
    , case state.route of
        Home -> map ScientistAction $ Scientist.view state.sScientists
        NotFound -> NotFound.view state
    ]
