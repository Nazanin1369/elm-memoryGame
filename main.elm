import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes
import StartApp.Simple exposing (start)
import List
import Card
import Debug


main =
  start { model = init, view = view, update = update }



type alias Model =
   List Card.Model

type Action
  = Do Card.Status

init: List Card.Model
init =
  [
    Card.initialModel,
    Card.initialModel,
    Card.initialModel,
    Card.initialModel
  ]

view : Signal.Address Action -> Model -> Html.Html
view address model =
  div []
    (List.map (\cModel -> Card.view (Signal.forwardTo address Do) cModel) model)



update: Action -> Model -> Model
update action model =
    case action of
      Do x -> List.map (\cModel -> Card.update x cModel) model
