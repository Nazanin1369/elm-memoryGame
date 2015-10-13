import Graphics.Element exposing (..)
import Signal.Time exposing (..)


main : Signal Element
main =
  Signal.map (\ele -> show ele)  (startTime)
