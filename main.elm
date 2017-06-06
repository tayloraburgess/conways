import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (map, repeat, range)
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

buildCell : Int -> Int -> Cell
buildCell y x =
  { x = x, y = y, status = Alive }

buildRow : Int -> Int -> Row
buildRow size y =
  map (buildCell y) (range 0 size)

buildBoard : Int -> Board
buildBoard size =
  map (buildRow size) (range 0 size) 

model =
  (buildBoard 25)

checkNeighbors : Board -> Cell -> CellStatus
checkNeighbors board cell =
  if cell.status == Alive then
    Dead
  else
    Alive

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
