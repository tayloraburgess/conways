import Html exposing (Html, beginnerProgram, div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List exposing (map)
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


model =
  [ [ { x = 0, y = 0, status = Alive }, { x = 1, y = 0, status = Dead } ]
  , [ { x = 0, y = 1, status = Dead }, { x = 1, y = 1, status = Alive } ]
  ]

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
