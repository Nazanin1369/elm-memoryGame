module MemoryGame where

import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Signal.Time exposing (..)
import Html.Attributes
import StartApp.Simple exposing (start)
import List
import Card
import Debug
import String
import Random
import Array
import Random.Array
import Now



main =
  start { model = init, view = view, update = update }
  



type alias Model = {
   cards: List Card.Model,
   score: Int
}


type Action
  = Do Int Card.Status

initSeed =
   round Now.loadTime

shuffle: List Card.Model -> List Card.Model
shuffle list =
  case Random.Array.shuffle (Random.initialSeed initSeed) (Array.fromList list) of
    (x, y) -> (Array.toList x)

init: Model
init =
  {
    cards = shuffle <|
              List.map (\index -> Card.initialModel ("images/" ++ (toString (index % 18)) ++ ".svg") index) [1..36],
    score = 0
  }


view: Signal.Address Action -> Model -> Html.Html
view address model =
  div [Html.Attributes.class "cardsContainer"]
    [
      div []
        [(Html.text ("Tries " ++ (toString model.score)))],
      div []
        (List.map (\cModel -> Card.view (Signal.forwardTo address (Do cModel.id)) cModel) model.cards)

    ]



openImages: Model -> List Card.Model
openImages model =
  List.foldr (
    \m i ->
      if Card.isOpen m then
        m :: i
      else
        i
  ) [] model.cards

areIdentical: List Card.Model -> Bool
areIdentical list =
  let
    f = List.head list
    l = List.length list
  in
    case (l, f) of
      (2, Just h) -> List.all (\x -> String.contains x.image h.image) list
      (_, _) -> False


lockIfIdentical: List Card.Model -> List Card.Model -> List Card.Model
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
      modelAfterLock = { model | cards <- (lockIfIdentical model.cards opened) }
    in
      case action of
        Do y x ->  { modelAfterLock | cards <-  List.map (\cModel ->
                                  case (openedCount, cModel.id) of
                                    (c, l) ->  if l == y then
                                                  Card.update x cModel
                                               else
                                                 if c == 2 then
                                                   Card.close cModel
                                                 else
                                                   cModel
                          ) modelAfterLock.cards,
                          score <- modelAfterLock.score + 1
                      }
