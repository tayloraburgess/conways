port module Conways exposing (..)

import Html exposing (Html, program, div, button, text, input)
import Html.Attributes exposing (type_, defaultValue)
import Html.Events exposing (onClick, onInput)
import List exposing (map, indexedMap, range, foldr, foldl, append)
import Tuple exposing (first, second)
import String exposing (toInt)
import Random exposing (Seed, bool, initialSeed, step)
import Time exposing (Time, millisecond, now, inSeconds, every)
import Result exposing (withDefault)


main =
  program { init = (model, Cmd.none), view = view, update = update, subscriptions = subscriptions }


type CellStatus =
  Alive | Dead

type Msg =
  Init | Redraw | ChangeSeed String | ChangeSize String | Generate Time | Render Time

type alias Cell =
  { x : Int, y : Int, status : CellStatus }

type alias Row =
  List Cell

type alias Board =
  List Row

type alias JSCell =
  { x : Int, y : Int, status : Bool }

type alias Model =
  { board : Board, seed: Int, size: Int }

port render : List JSCell -> Cmd msg

port reDraw : Int -> Cmd msg

checkBool : Int -> (Int, Bool) -> CellStatus -> CellStatus
checkBool coord bool status =
  if (first bool) == coord then
    if (second bool) == True then
      Alive
    else
      Dead
  else
    status

getBool : List (Int, Bool) -> Int -> CellStatus
getBool bools coord =
  foldr (\a -> \b -> (checkBool coord a b)) Dead bools

  
buildCell : List (Int, Bool) -> Int -> Int -> Cell
buildCell bools y x =
  { x = x, y = y, status = (getBool bools (x + y)) }

buildRow : List (Int, Bool) -> Int -> Int -> Row
buildRow bools size y =
  map (buildCell bools y) (range 0 size)

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
    map (buildRow bools size) (range 0 size) 

model =
  { board = (buildBoard 20 30), size = 20, seed = 30 }

checkCell : Cell -> Cell -> Int
checkCell cell currentCell =
  if currentCell.x == cell.x && currentCell.y == cell.y then
    0
  else if currentCell.x == cell.x - 1 && currentCell.y == cell.y - 1 && currentCell.status == Alive  then
    1
  else if currentCell.x == cell.x - 1 && currentCell.y == cell.y && currentCell.status == Alive then
    1
  else if currentCell.x == cell.x - 1 && currentCell.y == cell.y + 1 && currentCell.status == Alive then
    1
  else if currentCell.x == cell.x && currentCell.y == cell.y - 1 && currentCell.status == Alive then
    1
  else if currentCell.x == cell.x && currentCell.y == cell.y + 1 && currentCell.status == Alive then
    1
  else if currentCell.x == cell.x + 1 && currentCell.y == cell.y - 1 && currentCell.status == Alive then
    1
  else if currentCell.x == cell.x + 1 && currentCell.y == cell.y && currentCell.status == Alive then
    1
  else if currentCell.x == cell.x + 1 && currentCell.y == cell.y + 1 && currentCell.status == Alive then
    1
  else
    0

statusToBool : CellStatus -> Bool
statusToBool status =
  if status == Alive then
    True
  else
    False

cellToJS : Cell -> JSCell
cellToJS cell =
  {x = cell.x, y = cell.y, status = (statusToBool cell.status)}

boardToJS : Board -> List JSCell
boardToJS board =
  foldl (\a -> \b -> (append (map cellToJS a) b)) [] board 

  
checkRow : Cell -> Row -> Int -> Int 
checkRow cell row fold =
  foldr (\a -> \b -> (checkCell cell a) + b) fold row 

checkNeighbors : Board -> Cell -> CellStatus
checkNeighbors board cell =
  let
    neighbors = foldl (checkRow cell) 0 board
  in
    if cell.status == Alive && neighbors < 2 then
      Dead
    else if cell.status == Alive && neighbors == 2 || neighbors == 3 then
      Alive
    else if cell.status == Alive && neighbors > 3 then
      Dead
    else if cell.status == Dead && neighbors == 3 then
      Alive
    else
      Dead

update msg model =
  case msg of
    Init -> 
      (model, Cmd.none)
    Generate time ->
      let
        updateCell : Cell -> Cell
        updateCell cell =
          { x = cell.x, y = cell.y, status = (checkNeighbors model.board cell) }

        updateRow : Row -> Row
        updateRow row =
          map updateCell row
      in
        ({ board = (map updateRow model.board), size = model.size, seed = model.seed }, Cmd.none)
    Render time ->
      (model, render (boardToJS model.board))

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
    input [ type_ "number", defaultValue "20", onInput ChangeSize ] []
    , input [ type_ "number", defaultValue "30", onInput ChangeSeed ] []
    , button [ onClick Redraw ] [ text "Redraw Board!" ]
    ]
