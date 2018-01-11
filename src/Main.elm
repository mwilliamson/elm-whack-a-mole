import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { width : Int
  }
  
width : Model -> Int
width model = model.width


init : (Model, Cmd Msg)
init =
  (Model 3, Cmd.none)



-- UPDATE


type alias Msg = {}


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  let
    row = viewRow (List.repeat (width model) viewEmptySquare)
    grid = div [] (List.repeat (width model) row)
  in
  grid


viewRow : List (Html Msg) -> Html Msg
viewRow = div [style [("clear", "both")]]


viewEmptySquare : Html Msg
viewEmptySquare = viewSquare "#eee"


viewSquare : String -> Html Msg
viewSquare color =
  let width = "40px"
  in
  div
    [ style
      [ ("float", "left")
      , ("backgroundColor", color)
      , ("border", "1px solid #fff")
      , ("width", width)
      , ("height", width)
      ]
    ]
    []
