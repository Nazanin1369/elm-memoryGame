import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes
import StartApp.Simple exposing (start)
import List
import Card
import Debug
import String


main =
  start { model = init, view = view, update = update }



type alias Model =
   List Card.Model

type Action
  = Do Int Card.Status

init: List Card.Model
init =
  List.map (\index -> Card.initialModel ("images/" ++ (toString (index % 8)) ++ ".svg") index) [1..16]

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


openImages: Model -> List Card.Model
openImages model =
  List.foldr (
    \m i ->
      if Card.isOpen m then
        m :: i
      else
        i
  ) [] model

areIdentical: List Card.Model -> Bool
areIdentical list =
  let
    f = List.head list
    l = List.length list
  in
    case (l, f) of
      (2, Just h) -> List.all (\x -> String.contains x.image h.image) list
      (_, _) -> False


lockIfIdentical: Model -> List Card.Model -> Model
lockIfIdentical model list =
  let
    identical = areIdentical (list)
  in
      case identical of
        True -> List.map (\cmodel -> if Card.isOpen cmodel then
                                        Card.lock cmodel
                                     else
                                        cmodel
                         ) model

        False -> model


update: Action -> Model -> Model
update action model =
    let
      opened = openImages model
      openedCount = List.length (opened)
    in
      case action of
        Do y x -> (lockIfIdentical model opened) |>
                      List.map (\cModel ->
                              case (openedCount, cModel.id) of
                                (c, l) ->  if l == y then
                                              Card.update x cModel
                                           else
                                             if c == 2 then
                                               Card.close cModel
                                             else
                                               cModel
                      )
