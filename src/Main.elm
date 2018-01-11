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
  { activeRadius : Int
  }
  
activeWidth : Model -> Int
activeWidth model = model.activeRadius * 2 - 1

maxWidth : Model -> Int
maxWidth model = 5

gridIndices : Model -> List Int
gridIndices model = List.range 0 ((maxWidth model) - 1)

isActive : Model -> Int -> Int -> Bool
isActive model rowIndex columnIndex =
  isActiveIndex model rowIndex && isActiveIndex model columnIndex

isActiveIndex : Model -> Int -> Bool
isActiveIndex model index =
  let
    left = ((maxWidth model) - (activeWidth model)) // 2
    right = left + (activeWidth model) - 1
  in
    index >= left && index <= right

init : (Model, Cmd Msg)
init =
  ({ activeRadius = 2 }, Cmd.none)



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
  div [] (List.map (viewModelRow model) (gridIndices model))


viewModelRow : Model -> Int -> Html Msg
viewModelRow model rowIndex =
  viewRow (List.map (viewModelSquare model rowIndex) (gridIndices model))


viewModelSquare : Model -> Int -> Int -> Html Msg
viewModelSquare model rowIndex columnIndex =
  if isActive model rowIndex columnIndex then
    viewEmptySquare
  else
    viewUnusedSquare


viewRow : List (Html Msg) -> Html Msg
viewRow = div [style [("clear", "both")]]


viewEmptySquare : Html Msg
viewEmptySquare = viewSquare "#eee"


viewUnusedSquare : Html Msg
viewUnusedSquare = viewSquare "#fff"


viewSquare : String -> Html Msg
viewSquare color =
  let
    width = "40px"
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
