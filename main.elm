import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (map, range, foldr)
import Tuple exposing (first)
import Debug exposing (log)
import Time exposing (now, inSeconds)
import Css exposing (asPairs, display, height, width, backgroundColor, rgb, px, inlineBlock)


main =
  beginnerProgram { model = model, view = view, update = update }


type CellStatus =
  Alive | Dead

type Msg =
  Generate 

type alias Cell =
  { x : Int, y : Int, status : CellStatus }

type alias Row =
  List Cell

type alias Board =
  List Row

notRandomState : Int -> CellStatus
notRandomState seed =
  if seed % 3 == 0 then
    Alive
  else
    Dead

buildCell : Int -> Int -> Cell
buildCell y x =
  { x = x, y = y, status = (notRandomState (x + y)) }

buildRow : Int -> Int -> Row
buildRow size y =
  map (buildCell y) (range 0 size)

buildBoard : Int -> Board
buildBoard size =
  map (buildRow size) (range 0 size) 

model =
  (buildBoard 25)

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

  
checkRow : Cell -> Row -> Int -> Int 
checkRow cell row fold =
  foldr (\a -> \b -> (checkCell cell a) + b) fold row 

checkNeighbors : Board -> Cell -> CellStatus
checkNeighbors board cell =
  let
    neighbors = foldr (checkRow cell) 0 board
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
    Generate ->
      let
        updateCell : Cell -> Cell
        updateCell cell =
          { x = cell.x, y = cell.y, status = (checkNeighbors model cell) }

        updateRow : Row -> Row
        updateRow row =
          (map updateCell row)
      in
        (map updateRow model) 


styles =
  asPairs >> style

renderCell : Cell -> Html msg 
renderCell cell =
  if cell.status == Alive then
    div [ styles [ display inlineBlock, width (px 20), height (px 20), backgroundColor (rgb 255 0 0) ] ]  []
  else
    div [ styles [ display inlineBlock, width (px 20), height (px 20), backgroundColor (rgb 255 255 255) ] ]  []

renderRow : Row -> Html msg
renderRow row =
  div [ styles [ height (px 20) ] ] (map renderCell row)

view model =
  div []
  [ button [ onClick Generate ] [ text "Generate" ]
  , div [] (map renderRow model)
  ]
