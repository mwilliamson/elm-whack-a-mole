import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
import Time



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
  , tickIndex: Int
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
  ({ activeRadius = 2, tickIndex = 0 }, Cmd.none)



-- UPDATE


type Msg = Tick


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick -> ({ model | tickIndex = model.tickIndex + 1 }, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second (\_ -> Tick)



-- VIEW


type Square = UnusedSquare | EmptySquare
type alias Row = List Square
type alias Grid = List Row


render : Model -> Grid
render model =
  List.map (renderRow model) (gridIndices model)

renderRow : Model -> Int -> Row
renderRow model rowIndex =
  List.map (renderSquare model rowIndex) (gridIndices model)

renderSquare : Model -> Int -> Int -> Square
renderSquare model rowIndex columnIndex =
  if isActive model rowIndex columnIndex then
    EmptySquare
  else
    UnusedSquare


view : Model -> Html Msg
view model =
  viewGrid (render model)


viewGrid : Grid -> Html Msg
viewGrid grid =
  div [] (List.map viewGridRow grid)


viewGridRow : Row -> Html Msg
viewGridRow row =
  viewRow (List.map viewGridSquare row)


viewGridSquare : Square -> Html Msg
viewGridSquare square =
  case square of
    EmptySquare ->
      viewEmptySquare
    UnusedSquare ->
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
