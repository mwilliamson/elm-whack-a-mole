import Array
import Dict
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

personScore : Person -> Int
personScore person =
  case person of
    SuperBadPerson -> 200
    BadPerson -> 50
    GoodPerson -> -100

ticksPerStage = 10

superBadDuration = 5

type alias Stage =
  { radius : Int
  , badSquares : Int
  , goodSquares : Int
  , tickDuration : Time.Time
  }

stages =
  [ { radius = 2, badSquares = 2, goodSquares = 1, tickDuration = Time.second }
  , { radius = 3, badSquares = 3, goodSquares = 2, tickDuration = Time.second * 0.9 }
  , { radius = 4, badSquares = 4, goodSquares = 3, tickDuration = Time.second * 0.8 }
  , { radius = 5, badSquares = 5, goodSquares = 4, tickDuration = Time.second * 0.7 }
  ]

type Model
  = Uninitialised
  | Playing PlayingState

type alias ReadyState =
  { superBadTickIndex : Int
  }

type alias PlayingState =
  { tickIndex : Int
  , grid : Grid
  , score : Int
  , superBadTickIndex : Int
  }

type Person = GoodPerson | BadPerson | SuperBadPerson

type Square = UnusedSquare | EmptySquare | OccupiedSquare Person
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
init =
  let
    totalTicks = ticksPerStage * (List.length stages)
    lastPossibleTick = totalTicks - superBadDuration
  in
    (Uninitialised, Random.generate (Initialise) (Random.int 0 lastPossibleTick))


initialGrid : Stage -> Bool -> PlayingState -> Grid
initialGrid stage keepSuperBad previousState =
  let
    emptyGrid = List.map (initialRow stage) gridIndices
    remainingGrid =
      if keepSuperBad then
        List.filter
          (Tuple.second >> ((==) (OccupiedSquare SuperBadPerson)))
          (indexedSquares previousState.grid)
      else
        []
  in
    setSquares remainingGrid emptyGrid

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
  | Initialise Int
  | UpdateState PlayingState
  | HitSquare (Int, Int) Square


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Initialise superBadTickIndex ->
      let
        initialPlayingState =
          { tickIndex = 0
          , grid = [] 
          , superBadTickIndex = superBadTickIndex
          , score = 0}
      in
        ( model, nextState 0 initialPlayingState )
  
    Tick ->
      case model of
        Uninitialised ->
          (model, Cmd.none)
        Playing state ->
          let
            tickIndex = state.tickIndex + 1
          in
            ( model, nextState tickIndex state )
        
    UpdateState state ->
      ( Playing state, Cmd.none )
      
    HitSquare coordinates square ->
      case square of
        OccupiedSquare person -> 
          let
            updatePlayingState state =
              let
                score = state.score + (personScore person)
                grid = setSquare EmptySquare coordinates state.grid
              in
                { state | score = score, grid = grid }
                
          in ( mapPlayingState updatePlayingState model, Cmd.none )
        _ -> ( model, Cmd.none )


nextState : Int -> PlayingState -> Cmd Msg
nextState tickIndex state =
  case activeStage tickIndex of
    Just stage -> 
      let
        isSuperBadStartTick = state.superBadTickIndex == tickIndex
        keepSuperBad = tickIndex > state.superBadTickIndex && tickIndex < state.superBadTickIndex + superBadDuration
      
        grid = initialGrid stage keepSuperBad state
        emptySquares = findEmptySquareIndices grid
        badSquares = (List.repeat stage.badSquares (OccupiedSquare BadPerson))
        goodSquares = (List.repeat stage.goodSquares (OccupiedSquare GoodPerson))
        
        superBadSquares = if isSuperBadStartTick then [OccupiedSquare SuperBadPerson] else []
        filledSquares = badSquares ++ goodSquares ++ superBadSquares
        updateState availableSquares =
          { state
          | tickIndex = tickIndex
          , grid = setSquares (List.map2 (,) availableSquares filledSquares) grid
          }
      in Random.generate (updateState >> UpdateState) (shuffle emptySquares)
    Nothing ->
      Cmd.none
  


mapPlayingState : (PlayingState -> PlayingState) -> Model -> Model
mapPlayingState update model =
  case model of
    Playing state -> Playing (update state)
    _ -> model


setSquares : List ((Int, Int), Square) -> Grid -> Grid
setSquares newSquares grid =
  let
    squareLookup = Dict.fromList newSquares
  in
    mapSquares
      (\(coordinates, square) ->
        case Dict.get coordinates squareLookup of
          Just newSquare -> newSquare
          Nothing -> square
      )
      grid

setSquare : Square -> (Int, Int) -> Grid -> Grid
setSquare newSquare targetCoordinates =
  setSquares [(targetCoordinates, newSquare)]


mapSquares : (((Int, Int), Square) -> a) -> Grid -> List (List a)
mapSquares mapSquare grid = List.map
  (List.map mapSquare)
  (indexedGrid grid)


shuffle : List a -> Random.Generator (List a)
shuffle list =
  Random.map Array.toList (shuffleArray (Array.fromList list))


shuffleArray : Array.Array a -> Random.Generator (Array.Array a)
shuffleArray array =
  let
    lastIndex = (Array.length array) - 1
    range = List.range 0 (lastIndex - 1)
    indicesGenerator =
      flattenGenerators (List.map (\index -> Random.int index lastIndex) range)
    indicesToSwapGenerator =
      Random.map (List.map2 (,) range) indicesGenerator
  in
    Random.map (swapIndices array) indicesToSwapGenerator


swapIndices : Array.Array a -> List (Int, Int) -> Array.Array a
swapIndices array toSwaps =
  case toSwaps of
    ((firstIndex, secondIndex)::toSwapsTail) ->
      swapIndices (swap array firstIndex secondIndex) (List.drop 1 toSwaps)  
    
    [] -> array


swap : Array.Array a -> Int -> Int -> Array.Array a
swap array firstIndex secondIndex =
  let
    first = Array.get firstIndex array
    second = Array.get secondIndex array
  in
    case (first, second) of
      (Just first, Just second) ->
        Array.set firstIndex second (Array.set secondIndex first array)
      _ -> array


flattenGenerators : List (Random.Generator a) -> Random.Generator (List a)
flattenGenerators =
  foldGenerators (::) []


foldGenerators : (a -> b -> b) -> b -> List (Random.Generator a) -> Random.Generator b
foldGenerators f initial list =
  case list of
    head::tail -> Random.map2 f head (foldGenerators f initial tail)
    [] -> liftRandom initial
    

liftRandom : a -> Random.Generator a
liftRandom value =
  Random.map (\_ -> value) (Random.list 0 Random.bool)


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
  viewSquare (squareColor square) (HitSquare coordinates square)


squareColor : Square -> String
squareColor square =
  case square of
    EmptySquare -> "#eee"
    UnusedSquare -> "#fff"
    (OccupiedSquare BadPerson) -> "#900"
    (OccupiedSquare SuperBadPerson) -> "#c39"
    (OccupiedSquare GoodPerson) -> "#090"


viewRow : List (Html Msg) -> Html Msg
viewRow = div [style [("clear", "both")]]


viewSquare : String -> Msg -> Html Msg
viewSquare color handleClick =
  let
    width = "40px"
    squareStyle = style
      [ ("float", "left")
      , ("backgroundColor", color)
      , ("border", "1px solid #fff")
      , ("width", width)
      , ("height", width)
      ]
  in
    div
      [ squareStyle, onClick handleClick ]
      []
