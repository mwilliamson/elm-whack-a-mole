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
  { gridSize : GridSize
  , tickIndex : Int
  , grid : Grid
  }

type alias GridSize =
  { activeRadius : Int
  }
  
type Square = UnusedSquare | EmptySquare
type alias Row = List Square
type alias Grid = List Row
  
activeWidth : GridSize -> Int
activeWidth gridSize = radiusToWidth gridSize.activeRadius

maxRadius : GridSize -> Int
maxRadius gridSize = 5

maxWidth : GridSize -> Int
maxWidth gridSize = radiusToWidth (maxRadius gridSize)

radiusToWidth : Int -> Int
radiusToWidth radius = radius * 2 - 1

gridIndices : GridSize -> List Int
gridIndices gridSize = List.range 0 ((maxWidth gridSize) - 1)

isActive : GridSize -> Int -> Int -> Bool
isActive gridSize rowIndex columnIndex =
  isActiveIndex gridSize rowIndex && isActiveIndex gridSize columnIndex

isActiveIndex : GridSize -> Int -> Bool
isActiveIndex gridSize index =
  let
    left = ((maxWidth gridSize) - (activeWidth gridSize)) // 2
    right = left + (activeWidth gridSize) - 1
  in
    index >= left && index <= right

init : (Model, Cmd Msg)
init =
  let
    gridSize = { activeRadius = 2 }
  in
    ( { gridSize = gridSize
      , tickIndex = 0
      , grid = initialGrid gridSize
      }
    , Cmd.none
    )


initialGrid : GridSize -> Grid
initialGrid gridSize =
  List.map (initialRow gridSize) (gridIndices gridSize)

initialRow : GridSize -> Int -> Row
initialRow gridSize rowIndex =
  List.map (initialSquare gridSize rowIndex) (gridIndices gridSize)

initialSquare : GridSize -> Int -> Int -> Square
initialSquare gridSize rowIndex columnIndex =
  if isActive gridSize rowIndex columnIndex then
    EmptySquare
  else
    UnusedSquare


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


view : Model -> Html Msg
view model =
  viewGrid model.grid


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
