module MemoryGame where

import Html exposing (div, button, text, span, p, img)
import Html.Events exposing (onClick)
import Signal.Time exposing (..)
import Html.Attributes
import StartApp.Simple exposing (start)
import List
import Card
import Debug exposing (log)
import String
import Random
import Array
import Random.Array
import Now



main =
  start { model = init, view = view, update = update }


type alias Model = {
   cards: List Card.Model,
   matched_pair: Int,
   score: Int,
   rows: Int,
   columns: Int
}


type Action
  = Do Int Card.Status
  | Restrat


type alias Time = Float

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
              List.map (\index -> Card.initialModel ("images/" ++ (toString (index % 8)) ++ ".png") index) [1..16],
    score = 0,
    matched_pair = 0,
    rows = 4,
    columns = 4
  }

cardWidthStyle : Model -> Html.Attribute
cardWidthStyle model =
  let
    w = toString (model.columns * 80) ++ "px"
  in
  Html.Attributes.style <|
  [
    ("width", w)
  ]

view: Signal.Address Action -> Model -> Html.Html
view address model =
  let
    maxCount = List.length model.cards
  in
    if model.matched_pair == maxCount then
      div [Html.Attributes.class "winContainer"]
      [
        p [] [
          text "You Won!",
          img [Html.Attributes.src "images/halloween178.svg"] []
        ],
        p [] [
          --text toString (model.matched_pair - model.score),
          span [] [(Html.text ("Score: " ++ toString (model.matched_pair * 50 - model.score)))]
        ],
        button [onClick address Restrat] [Html.text "Restrat"]
      ]
    else
      div [] [
       div [Html.Attributes.class  "infoContainer"] [
        p [] [
          Html.text "Tries ",
          span [] [(Html.text (toString model.score))]
        ],
        p [] [
          Html.text "Matched ",
            span [] [(Html.text (toString ((toFloat model.matched_pair) / 2)  ++ " / " ++ toString ( toFloat model.rows * toFloat model.columns / 2)))]
        ]
       ],
       div [Html.Attributes.class "cardsContainer", cardWidthStyle model]
          [
           div []
            (List.map (\cModel -> Card.view (Signal.forwardTo address (Do cModel.id)) cModel) model.cards)
          ]
      ]



getOpenCards: Model -> List Card.Model
getOpenCards model =
  List.foldr (
    \m i ->
      if Card.isOpen m then
        m :: i
      else
        i
  ) [] model.cards


countOpenAllCards: Model -> String
countOpenAllCards model =
  List.foldr (
    \m i ->
      if Card.isOpen m then
        i ++ "1 "
      else
        i
  ) "" model.cards

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
        True -> model |> List.map (\cmodel -> if Card.isOpen cmodel then
                                        Card.lock cmodel
                                     else
                                        cmodel
                         )
        False ->  model



updateCardStatus: Model -> Model
updateCardStatus model =
  let
      opened = getOpenCards model
  in
    { model | cards <- (lockIfIdentical model.cards opened) }


updateCardById: Card.Status -> Int -> Model -> Model
updateCardById status id model =
    { model | cards <- List.map (\cItem ->
                        if cItem.id == id then
                            Card.update status cItem
                        else
                          cItem
                      ) model.cards,
              score <- model.score + 1
    }



countOpenCards: Model -> Int
countOpenCards model =
     List.foldr (\cItem o ->
                  if Card.isOpen cItem then
                    o + 1
                  else
                    o
                ) 0  model.cards


closeAllCards: Int ->Model -> Model
closeAllCards id model =
  let
    count = countOpenCards model
  in
    if count > 2 then
      {model | cards <- List.map  (\cItem ->
                      case (Card.isOpen cItem, id) of
                        (true, x) -> if cItem.id == x then
                                     cItem
                                    else
                                     Card.close cItem
                        (false, _) -> cItem
              ) model.cards
      }
    else
      model


checkAndLock: Model -> Model
checkAndLock model =
  let
    openCards = getOpenCards model
    count = List.length openCards
    same = areIdentical openCards
  in
    if count == 2 then
      case same of
        True -> { model | cards <- List.map  (\cItem ->
                                          if Card.isOpen cItem then
                                            Card.lock cItem
                                          else
                                            cItem
                                    ) model.cards,
                         matched_pair <- model.matched_pair + 2}
        False -> model
    else
      model


update: Action -> Model -> Model
update action model =
      case action of
        Restrat -> init
        --Do Int Card.status
        Do y x -> model |> updateCardById x y
                        |> checkAndLock
                        |> closeAllCards y
