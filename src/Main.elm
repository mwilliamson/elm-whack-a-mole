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

hitBadSquareScore = 50

type alias Model =
  { gridSize : GridSize
  , tickIndex : Int
  , grid : Grid
  , score : Int
  }

type alias GridSize =
  { activeRadius : Int
  }
  
type Square = UnusedSquare | EmptySquare | BadSquare
type alias Row = List Square
type alias Grid = List Row
  
activeRadius : GridSize -> Int
activeRadius gridSize = gridSize.activeRadius
  
activeWidth : GridSize -> Int
activeWidth gridSize = radiusToWidth (activeRadius gridSize)

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
    left = (maxRadius gridSize) - (activeRadius gridSize)
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
      , score = 0
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


findEmptySquareIndices : Grid -> List (Int, Int)
findEmptySquareIndices grid = List.filterMap
  (\(coordinates, square) ->
    case square of
      EmptySquare -> Just coordinates
      _ -> Nothing
  )
  (indexedSquares grid)

indexedSquares : Grid -> List ((Int, Int), Square)
indexedSquares grid = indexedGrid grid |> List.concat


indexedGrid : Grid -> List (List ((Int, Int), Square))
indexedGrid grid = List.indexedMap
  (\rowIndex -> \row ->
    List.indexedMap
      (\columnIndex -> \square ->
        ((rowIndex, columnIndex), square)
      )
      row
  )
  grid

-- UPDATE


type Msg
  = Tick
  | UpdateGrid Grid
  | HitBadSquare (Int, Int)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      ( { model | tickIndex = model.tickIndex + 1 }
      , Random.generate ((maybeSetBadSquare (initialGrid model.gridSize)) >> UpdateGrid) (selectRandom (findEmptySquareIndices model.grid))
      )
    UpdateGrid grid ->
      ( { model | grid = grid }, Cmd.none)
    HitBadSquare coordinates ->
      let
        score = model.score + hitBadSquareScore
      in
        ( { model | score = score, grid = setSquare EmptySquare model.grid coordinates }, Cmd.none)

maybeSetBadSquare : Grid -> Maybe (Int, Int) -> Grid
maybeSetBadSquare grid coordinates =
  case coordinates of
    Just c -> setBadSquare grid c
    Nothing -> grid

setBadSquare : Grid -> (Int, Int) -> Grid
setBadSquare = setSquare BadSquare

setSquare : Square -> Grid -> (Int, Int) -> Grid
setSquare newSquare grid targetCoordinates = List.map
  (\row ->
    List.map
      (\(coordinates, square) ->
        if coordinates == targetCoordinates then
          newSquare
        else
          square
      )
      row
  )
  (indexedGrid grid)


selectRandom : List a -> Random.Generator (Maybe a)
selectRandom list =
  Random.map (nth list) (Random.int 0 ((List.length list) - 1))


nth : List a -> Int -> Maybe a
nth list index = List.head (List.drop index list)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every Time.second (\_ -> Tick)



-- VIEW


view : Model -> Html Msg
view model =
  div
    []
    [ (text ("Score: " ++ (toString model.score)))
    , viewGrid model.grid
    ]


viewGrid : Grid -> Html Msg
viewGrid grid =
  div
    []
    (List.map viewGridRow (indexedGrid grid))


viewGridRow : List ((Int, Int), Square) -> Html Msg
viewGridRow row =
  viewRow (List.map (uncurry viewGridSquare) row)


viewGridSquare : (Int, Int) -> Square -> Html Msg
viewGridSquare coordinates square =
  case square of
    EmptySquare ->
      viewEmptySquare
    UnusedSquare ->
      viewUnusedSquare
    BadSquare ->
      viewBadSquare (HitBadSquare coordinates)


viewRow : List (Html Msg) -> Html Msg
viewRow = div [style [("clear", "both")]]


viewEmptySquare : Html Msg
viewEmptySquare = viewSquare "#eee" Nothing


viewUnusedSquare : Html Msg
viewUnusedSquare = viewSquare "#fff" Nothing


viewBadSquare : Msg -> Html Msg
viewBadSquare onClick = viewSquare "#900" (Just onClick)


viewSquare : String -> (Maybe Msg) -> Html Msg
viewSquare color maybeOnClick =
  let
    width = "40px"
    squareStyle = style
      [ ("float", "left")
      , ("backgroundColor", color)
      , ("border", "1px solid #fff")
      , ("width", width)
      , ("height", width)
      ]
    handlers = case maybeOnClick of
      Just handler -> [ onClick handler ]
      Nothing -> []
  in
    div
      ([ squareStyle ] ++ handlers)
      []
