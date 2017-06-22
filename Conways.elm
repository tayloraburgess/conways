port module Conways exposing (..)

import Html exposing (Html, program, div, button, text, input)
import Html.Attributes exposing (type_, defaultValue)
import Html.Events exposing (onClick, onInput)
import List exposing (map, indexedMap, range, foldr, foldl, append, member)
import Tuple exposing (first, second)
import String exposing (toInt)
import Random exposing (Seed, bool, initialSeed, step)
import Time exposing (Time, millisecond, now, inSeconds, every)
import Result exposing (withDefault)
import Debug exposing (log)


main =
  program { init = (model, Cmd.none), view = view, update = update, subscriptions = subscriptions }


type Msg =
  Init | Redraw | ChangeSeed String | ChangeSize String | Generate Time | Render Time

type alias Cell =
  { x : Int, y : Int, status : Bool }

type alias Board =
  List Cell 

type alias Model =
  { board : Board, seed: Int, size: Int }

port render : Board -> Cmd msg

port reDraw : Int -> Cmd msg

getBool : List (Int, Bool) -> Int -> Bool
getBool bools coord =
  member (coord, True) bools


buildCell : List (Int, Bool) -> Int -> Int -> Cell
buildCell bools size idx =
  let
      x = idx // size 
      y = idx % size
  in
      { x = x, y = y, status = (getBool bools (x + y)) }

buildBoardList : List (Int, Bool) -> Int -> Board 
buildBoardList bools size =
  map (buildCell bools size) (range 0 (size * size))

generateBool : Int -> (List Bool, Seed) -> (List Bool, Seed)
generateBool num tuple =
  let
      newSeed = step bool (second tuple)
  in
      (append (first tuple) [(first newSeed)], (second newSeed))


buildBoard : Int -> Int -> Board
buildBoard size seed =
  let
      bools = indexedMap (\a -> \b -> (a, b)) (first(foldl generateBool ([], (initialSeed seed)) (range 0 (size * size))))
  in
      buildBoardList bools size 

model =
  { board = (buildBoard 5 30), size = 5, seed = 30 }

countNeighbors : Cell -> Board -> Int
countNeighbors cell board =
  let
      checkList = [
        { x = cell.x - 1, y = cell.y - 1, status = True }
        , { x = cell.x - 1, y = cell.y, status = True }
        , { x = cell.x - 1, y = cell.y + 1, status = True }
        , { x = cell.x, y = cell.y - 1, status = True }
        , { x = cell.x, y = cell.y + 1, status = True }
        , { x = cell.x + 1, y = cell.y - 1, status = True }
        , { x = cell.x + 1, y = cell.y, status = True }
        , { x = cell.x + 1, y = cell.y + 1, status = True }
        ]
  in
      foldl (\a -> \b -> b + (neighborIncrement (member a board))) 0 checkList 

neighborIncrement : Bool -> Int 
neighborIncrement bool =
  if bool == True then
    1
  else
    0

checkNeighbors : Board -> Cell -> Bool
checkNeighbors board cell =
  let
      neighbors = (countNeighbors cell board) 
  in
      if cell.status == True && neighbors < 2 then
        False 
      else if cell.status == True && neighbors == 2 || neighbors == 3 then
        True
      else if cell.status == True && neighbors > 3 then
        False
      else if cell.status == False && neighbors == 3 then
        True
      else
        False

update msg model =
  case msg of
    Init -> 
      (model, Cmd.none)
    Generate time ->
      let
          updateCell : Cell -> Cell
          updateCell cell =
            { x = cell.x, y = cell.y, status = (checkNeighbors model.board cell) }
      in
          ({ board = (map updateCell model.board), size = model.size, seed = model.seed }, Cmd.none)
    Render time ->
      (model, render model.board)

    ChangeSize newSize ->
      ({board = model.board, size = (withDefault 0 (toInt newSize)), seed = model.seed}, Cmd.none)

    ChangeSeed newSeed ->
      ({board = model.board, size = model.size, seed = (withDefault 0 (toInt newSeed))}, Cmd.none)

    Redraw ->
      ({board = (buildBoard model.size model.seed), size = model.size, seed = model.seed }, reDraw model.size)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
  [ every (100 * millisecond) Generate
  , every (100 * millisecond) Render 
  ]

view model =
  div [] [ 
    input [ type_ "number", defaultValue "5", onInput ChangeSize ] []
    , input [ type_ "number", defaultValue "30", onInput ChangeSeed ] []
    , button [ onClick Redraw ] [ text "Redraw Board!" ]
    ]
