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
ticksPerStage = 10

type alias Stage =
  { radius : Int
  , tickDuration : Time.Time
  }

stages =
  [ { radius = 2, tickDuration = Time.second }
  , { radius = 3, tickDuration = Time.second * 0.9 }
  , { radius = 4, tickDuration = Time.second * 0.8 }
  , { radius = 5, tickDuration = Time.second * 0.7 }
  ]

type Model
  = Uninitialised
  | Playing PlayingState

type alias PlayingState =
  { tickIndex : Int
  , grid : Grid
  , score : Int
  }

type Square = UnusedSquare | EmptySquare | BadSquare
type alias Row = List Square
type alias Grid = List Row

activeStage : Int -> Maybe Stage
activeStage tickIndex =
  let 
    stageIndex = tickIndex // ticksPerStage
  in
    nth stages stageIndex

activeRadius : Stage -> Int
activeRadius stage = stage.radius
  
activeWidth : Stage -> Int
activeWidth stage = radiusToWidth (activeRadius stage)

maxRadius : Int
maxRadius = 
  let
    radii = (List.map .radius stages)
  in
    Maybe.withDefault 0 (List.maximum radii)

maxWidth : Int
maxWidth = radiusToWidth maxRadius

radiusToWidth : Int -> Int
radiusToWidth radius = radius * 2 - 1

gridIndices : List Int
gridIndices = List.range 0 (maxWidth - 1)

isActive : Stage -> Int -> Int -> Bool
isActive stage rowIndex columnIndex =
  isActiveIndex stage rowIndex && isActiveIndex stage columnIndex

isActiveIndex : Stage -> Int -> Bool
isActiveIndex stage index =
  let
    left = maxRadius - (activeRadius stage)
    right = left + (activeWidth stage) - 1
  in
    index >= left && index <= right

init : (Model, Cmd Msg)
init = update Tick Uninitialised


initialGrid : Stage -> Grid
initialGrid stage =
  List.map (initialRow stage) gridIndices

initialRow : Stage -> Int -> Row
initialRow gridSize rowIndex =
  List.map (initialSquare gridSize rowIndex) gridIndices

initialSquare : Stage -> Int -> Int -> Square
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
  | UpdateState PlayingState
  | HitBadSquare (Int, Int)


getState : (PlayingState -> a) -> a -> Model -> a
getState get default model =
  case model of
    Playing state -> (get state)
    _ -> default

nextTickIndex : Model -> Int
nextTickIndex model =
  getState (.tickIndex >> ((+) 1)) 0 model

currentScore : Model -> Int
currentScore model =
  getState .score 0 model


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick ->
      let
        tickIndex = nextTickIndex model
      in
        case (activeStage tickIndex) of
          Just stage ->
            let
              grid = initialGrid stage
              updateState badSquare =
                { tickIndex = tickIndex,
                  grid = (maybeSetBadSquare grid badSquare),
                  score = (currentScore model)
                }
            in
              ( model
              , Random.generate (updateState >> UpdateState) (selectRandomEmptySquare grid)
              )
          Nothing -> (model, Cmd.none)
        
    UpdateState state ->
      ( Playing state, Cmd.none)
      
    HitBadSquare coordinates ->
      let
        updatePlayingState state =
          let
            score = (currentScore model) + hitBadSquareScore
            grid = setSquare EmptySquare state.grid coordinates
          in
            { state | score = score, grid = grid }
            
      in ( mapPlayingState updatePlayingState model, Cmd.none )

mapPlayingState : (PlayingState -> PlayingState) -> Model -> Model
mapPlayingState update model =
  case model of
    Playing state -> Playing (update state)
    _ -> model

maybeSetBadSquare : Grid -> Maybe (Int, Int) -> Grid
maybeSetBadSquare grid coordinates =
  case coordinates of
    Just c -> setBadSquare grid c
    Nothing -> grid

setBadSquare : Grid -> (Int, Int) -> Grid
setBadSquare = setSquare BadSquare

setSquare : Square -> Grid -> (Int, Int) -> Grid
setSquare newSquare grid targetCoordinates = mapSquares
  (\(coordinates, square) ->
    if coordinates == targetCoordinates then
      newSquare
    else
      square
  )
  grid


mapSquares : (((Int, Int), Square) -> a) -> Grid -> List (List a)
mapSquares mapSquare grid = List.map
  (List.map mapSquare)
  (indexedGrid grid)


selectRandomEmptySquare : Grid -> Random.Generator (Maybe (Int, Int))
selectRandomEmptySquare grid =
  selectRandom (findEmptySquareIndices grid)


selectRandom : List a -> Random.Generator (Maybe a)
selectRandom list =
  Random.map (nth list) (Random.int 0 ((List.length list) - 1))


nth : List a -> Int -> Maybe a
nth list index = List.head (List.drop index list)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    Playing state -> 
      case activeStage state.tickIndex of
        Just stage -> Time.every stage.tickDuration (\_ -> Tick)
        Nothing -> Sub.none
    Uninitialised -> Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  case model of
    Uninitialised -> div [] []
    Playing state ->
      div
        []
        [ (text ("Score: " ++ (toString state.score)))
        , viewGrid state.grid
        ]


viewGrid : Grid -> Html Msg
viewGrid grid =
  let
    squares = mapSquares
      (uncurry viewGridSquare)
      grid
  in
    div
      []
      (List.map viewRow squares)


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
