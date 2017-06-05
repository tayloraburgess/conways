import Html exposing (beginnerProgram, div, text)
import Html.Attributes exposing (style)
import Css exposing (asPairs, height, width, backgroundColor, rgb, px)


main =
  beginnerProgram { model = 0, view = view, update = update }

type Msg = Increment | Decrement


update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1

styles =
  asPairs >> style

view model =
  div [ styles [ backgroundColor (rgb 255 0 0), width (px 50), height (px 50)] ] [ text "testing" ]
