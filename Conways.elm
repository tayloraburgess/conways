port module Conways exposing (..)

import Html exposing (Html, program, div, button, text)
import Html.Events exposing (onClick)
import List exposing (map, indexedMap, range, foldr, foldl, append)
import Tuple exposing (first, second)
import Debug exposing (log)
import Random exposing (Seed, bool, initialSeed, step)
import Time exposing (Time, millisecond, now, inSeconds, every)
import Css exposing (asPairs, display, height, width, backgroundColor, rgb, px, inlineBlock)


main =
  program { init = (model, Cmd.none), view = view, update = update, subscriptions = subscriptions }


type CellStatus =
  Alive | Dead

type Msg =
  Init | Generate Time | Render Time

type alias Cell =
  { x : Int, y : Int, status : CellStatus }

type alias Row =
  List Cell

type alias Board =
  List Row

type alias JSCell =
  { x: Int, y : Int, status : Bool }

port render : List JSCell -> Cmd msg

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
  (buildBoard 20 30)

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
          { x = cell.x, y = cell.y, status = (checkNeighbors model cell) }

        updateRow : Row -> Row
        updateRow row =
          (map updateCell row)
      in
        ((map updateRow model), Cmd.none)
    Render time ->
      (model, render (boardToJS model))


subscriptions : Board -> Sub Msg
subscriptions model =
  Sub.batch
  [ every (100 * millisecond) Generate
  , every (100 * millisecond) Render 
  ]

view model =
  div [] [ text "placeholder" ] 
