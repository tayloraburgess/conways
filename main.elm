import Html exposing (Html, beginnerProgram, div, text)
import Html.Attributes exposing (style)
import List exposing (map)
import Css exposing (asPairs, display, height, width, backgroundColor, rgb, px, inlineBlock)


main =
  beginnerProgram { model = model, view = view, update = update }


type Cell =
  Alive | Dead

type alias Row =
  List Cell

type alias Board =
  List Row

type Msg = Nothing 

model =
  [ [ Alive, Dead ], [ Dead, Alive ] ]


update msg model =
  case msg of
    Nothing ->
      model 


styles =
  asPairs >> style

renderCell : Cell -> Html msg 
renderCell cell =
  if cell == Alive then
    div [ styles [ display inlineBlock, width (px 20), height (px 20), backgroundColor (rgb 255 0 0) ] ]  [ ]
  else
    div [ styles [ display inlineBlock, width (px 20), height (px 20), backgroundColor (rgb 255 255 255) ] ]  [ ]

renderRow : Row -> Html msg
renderRow row =
  div [ styles [ height (px 20) ] ] (map renderCell row)

view model =
  div [ ] (map renderRow model)
