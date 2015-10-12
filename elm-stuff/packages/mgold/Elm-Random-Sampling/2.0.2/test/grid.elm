import Random.Array
import Array
import Random
import Text
import List
import Dict
import Graphics.Element exposing (Element, flow, right, down, spacer, show)
import Graphics.Collage exposing (toForm, move, collage, group, circle, filled)
import Color exposing (..)
import String

arr = Array.fromList [0..10]

start  = 6000
trials = 10000

runs : List (List Int)
runs = List.scanl (\_ (_, seed) -> Random.Array.shuffle seed arr) (Array.empty, Random.initialSeed start) [1..trials]
        |> List.map (fst >> Array.toList)

type alias Multiset a = Dict.Dict a Int
multiset : List comparable -> Multiset comparable
multiset xs = let incr m_int = case m_int of
                                Just n -> Just <| n+1
                                Nothing -> Just 1
                  step x mset = Dict.update x incr mset
              in List.foldl step Dict.empty xs

table : Dict.Dict comparable b -> Element
table d = let row (k,v) = flow right [show v, spacer 20 20, show k]
          in flow down <| List.map row (Dict.toList d)

grid : Multiset (Int,Int)
grid = multiset <| List.concatMap (List.indexedMap (,)) runs

grid_viz : Multiset (Int,Int) -> Element
grid_viz g = let
    min = List.minimum (Dict.values g) |> Maybe.withDefault -1 |> toFloat
    max = List.maximum (Dict.values g) |> Maybe.withDefault -1 |> toFloat
    normalize = linear (min, max) (1,0)
    colors = toFloat >> normalize >> color darkCharcoal lightGrey
    forms = g |> Dict.toList |> List.concatMap (\((i,j), v) ->
        [circle 10 |> filled (colors v) |> move (50*toFloat i, 50*toFloat j)
        , show v |> toForm |> move (50*toFloat i, -18 + 50*toFloat j)])
  in collage 600 600 [group forms |> move (-250, -250)]


main = flow down[show <| """
This is a visualization of the distribution of the shuffle for arrays. The
vertical axis shows the starting index of an element; top row is zero. The
horiztonal axis is the end location of an element. The numbers and shading
indicate how many times an element started in the position indicated by its row
and moved to the position indicated by its column. There are """ ++ (toString trials) ++ " trials."
    , grid_viz grid]

linear (d0,d1) (r0,r1) val = ((val - d0)/(d1 - d0)) * (r1 - r0) |> (+) r0 

-- https://github.com/Dandandan/Easing/blob/master/Easing.elm
float from to v =
    from + (to - from) * v

color : Color -> Color -> Float -> Color
color from to v =
    let
        (rgb1, rgb2)     = (toRgb from, toRgb to)
        (r1, g1, b1, a1) = (rgb1.red, rgb1.green, rgb1.blue, rgb1.alpha)
        (r2, g2, b2, a2) = (rgb2.red, rgb2.green, rgb2.blue, rgb2.alpha)
        float' from to v = round (float (toFloat from) (toFloat to) v)
    in rgba (float' r1 r2 v) (float' g1 g2 v) (float' b1 b2 v) (float a1 a2 v)
