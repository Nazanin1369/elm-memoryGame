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
  = Do Int Card.Status

init: List Card.Model
init =
  List.map (\index -> Card.initialModel ((toString (index % 8)) ++ ".svg") index) [1..16]

containerStyle : Html.Attribute
containerStyle =
    Html.Attributes.style <|
      [
        ("width", "290px"),
        ("height", "290px"),
        ("margin", "80px 500px 0px 500px")
      ]


view: Signal.Address Action -> Model -> Html.Html
view address model =
  div [containerStyle]
    (List.map (\cModel -> Card.view (Signal.forwardTo address (Do cModel.id)) cModel) model)


openImagesCount:  Model -> Int
openImagesCount model =
  List.foldr (
    \m i ->
      Card.incIfOpen m i
  ) 0 model

update: Action -> Model -> Model
update action model =
    let
      openedCount = openImagesCount model
    in
      case action of
        Do y x -> List.map (\cModel ->
                              case (openedCount, cModel.id) of
                                (c, l) ->  if l == y then
                                              Card.update x cModel
                                           else
                                             if c == 2 then
                                               Card.close cModel
                                             else
                                               cModel
                           ) model
