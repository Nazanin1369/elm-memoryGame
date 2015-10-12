module Card (Model, initialModel, update, view, Status) where

import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes
import StartApp.Simple exposing (start)

type alias Model =
  { status : Status,
    image : String
  }

type Status
    = Opened
    | Closed


type Action = Flip

initialModel =
  { status = Closed,
    image = "1.svg"
  }

toImage: Model -> Html.Attribute
toImage model =
  Html.Attributes.src <|
    case model.status of
      Closed -> "back.svg"

      Opened -> model.image


imageStyle : Html.Attribute
imageStyle =
  Html.Attributes.style <|
  [
    ("width", "50px"),
    ("height", "50px"),
    ("padding", "10px")
  ]

view : Signal.Address Status -> Model -> Html.Html
view address model =
  div [] [ div [onClick address model.status] [  Html.img [ toImage model, imageStyle] [] ] ]

update: Status -> Model -> Model
update action model =
  case action of
    Closed -> { model | status <- Opened}
    Opened -> { model | status <- Closed}
