module Card (Model, initialModel, update, view, Status, isOpen, close, lock, isLocked) where

import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Html.Attributes
import StartApp.Simple exposing (start)

type alias Model =
  { status : Status,
    image : String,
    id : Int
  }

type Status
    = Opened
    | Closed
    | Locked


type Action = Flip

initialModel: String -> Int -> Model
initialModel img id =
  { status = Closed,
    image = img,
    id =  id
  }

isOpen: Model -> Bool
isOpen model =
    case model.status of
      Opened -> True
      _ -> False


isLocked: Model -> Bool
isLocked model =
    case model.status of
      Locked -> True
      _ -> False


toImage: Model -> Html.Attribute
toImage model =
  Html.Attributes.src <|
    case model.status of
      Closed -> "images/back.svg"
      _   -> model.image

imageContainerStyle : Html.Attribute
imageContainerStyle =
  Html.Attributes.style <|
  [
    ("float", "left")
  ]

imageStyle : Html.Attribute
imageStyle =
  Html.Attributes.style <|
  [
    ("width", "80px"),
    ("height", "80px"),
    ("padding", "5px"),
    ("position", "relative"),
    ("display", "block")
  ]

view : Signal.Address Status -> Model -> Html.Html
view address model =
--  div [ Html.Attributes.class "flip-container"] [
  --  div [Html.Attributes.class "flipper", Html.Attributes.id "flipper"] [
    --  div [Html.Attributes.class "front", onClick address model.status] [Html.img [Html.Attributes.src "images/back.svg", imageStyle] []],
      --div [Html.Attributes.class "back", onClick address model.status] [ Html.img [ toImage model] [] ]
    --]
  --]
  div [Html.Attributes.class "flipper", Html.Attributes.id "flipper"] [
    Html.span [imageContainerStyle] [ Html.span [onClick address model.status] [  Html.img [ toImage model, imageStyle] [] ] ]
  ]

update: Status -> Model -> Model
update action model =
  case action of
    Locked -> model
    Closed -> { model | status <- Opened}
    Opened -> { model | status <- Closed}


close: Model -> Model
close model =
  case model.status of
    Locked -> model
    _ -> { model | status <- Closed}


lock: Model -> Model
lock model =
  { model | status <- Locked }
