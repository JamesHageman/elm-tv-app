port module Components.App exposing (init, view, update, subscriptions)

import Html exposing (Html, div, span, button, text, h3, a)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, classList, style, disabled, href, target)
import Html.App as App
import Http
import Task exposing (..)

import Json.Decode as Json exposing (int, string)
import Json.Decode.Pipeline exposing (decode, required)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Episode = {
  id : Int,
  name : String,
  season : Int,
  number : Int,
  url : String
}


type alias Season = {
  season : Int,
  episodes : List Episode
}

type alias User = {
  name : String,
  id : Int,
  lastWatchedEpisode : (Int, Int)
}

type alias State = {
  episodes : List Episode,
  error : Maybe Http.Error,
  currentUserId : Maybe Int,
  watchers : List User
}

type alias Model = {
  state : State,
  undoStack : List State,
  redoStack : List State
}

type InteractMsg =
  SelectEpisode Episode User
  | UnSelectEpisode Episode User

type Msg =
  LoadEpisodes (List Episode)
  | LoadSession FirebaseSession
  | Error Http.Error
  | PushState State
  | Undo
  | Redo
  | Interact InteractMsg
  | SelectUser Int


initUser : String -> Int -> (Int, Int) -> User
initUser name id lastWatchedEpisode =
  User name id lastWatchedEpisode


loadEpisodes : String -> Task Http.Error (List Episode)
loadEpisodes show =
  Http.get decodeEpisodes
    <| "http://api.tvmaze.com/shows/" ++ show ++ "/episodes"


decodeEpisode : Json.Decoder Episode
decodeEpisode =
  decode Episode
    |> required "id" int
    |> required "name" string
    |> required "season" int
    |> required "number" int
    |> required "url" string


decodeEpisodes : Json.Decoder (List Episode)
decodeEpisodes =
  Json.list decodeEpisode


initState : State
initState =
  { episodes = []
  , error = Nothing
  , currentUserId = Nothing
  , watchers = []
  }


init : (Model, Cmd Msg)
init =
  { state = initState
  , undoStack = []
  , redoStack = []
  } ! []


pushState : State -> Cmd Msg
pushState state =
  let
    pushToUndoStack = Task.succeed (PushState state)
  in
    Task.perform identity identity pushToUndoStack


findLastEpisodeNumber : List Episode -> Int -> Int
findLastEpisodeNumber episodes season =
  episodes
    |> List.filter (.season >> (==) season)
    |> (\list -> List.drop ((List.length list) - 1) list)
    |> List.head
    |> Maybe.map .number
    |> Maybe.withDefault 0

updateState : Msg -> State -> (State, Cmd Msg)
updateState msg state =
  case msg of
    LoadEpisodes episodes ->
      { state | episodes = episodes, error = Nothing } ! []

    LoadSession { watchers, showId } ->
      { state | watchers = watchers } ! [
        Task.perform Error LoadEpisodes (loadEpisodes showId)
      ]

    Error err ->
      { state | error = Just err } ! []

    SelectUser id ->
      { state | currentUserId = Just id } ! []

    Interact msg ->
      let
        (newState, stateFx) = updateFromInteraction msg state
      in
        newState ! [ stateFx, pushState state, setFirebaseState { session = "1", watchers = newState.watchers } ]

    _ -> state ! []

updateFromInteraction : InteractMsg -> State -> (State, Cmd Msg)
updateFromInteraction msg state =
  case msg of
    SelectEpisode episode watcher ->
      let
        episodeTuple = (episode.season, episode.number)
        watchers = state.watchers
          |> List.map
            (\user ->
              if user.id == watcher.id then
                { user | lastWatchedEpisode = episodeTuple }
                else user)
      in
        { state | watchers = watchers } ! []

    UnSelectEpisode episode watcher ->
      let
        { season, number } = episode
        (newSeason, newNumber) = if number > 1 then
          (season, number - 1)
          else
            (season - 1, findLastEpisodeNumber state.episodes (season - 1))

        _ = Debug.log "new" (newSeason, newNumber)

      in
        state ! []

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    PushState state ->
      { model |
        undoStack = state :: model.undoStack
      , redoStack = []
      } ! []

    Undo ->
      let
        { state, undoStack, redoStack } = model
        lastState = List.head undoStack
      in
        case lastState of
          Nothing -> model ! []

          Just rollBackState ->
            { model |
              state = rollBackState,
              undoStack = (List.tail undoStack |> Maybe.withDefault []),
              redoStack = state :: redoStack
            } ! [ setFirebaseState { session = "1", watchers = rollBackState.watchers } ]

    Redo ->
      let
        { state, undoStack, redoStack } = model

        rollForwardState = List.head redoStack
      in
        case rollForwardState of
          Nothing -> model ! []

          Just rollForwardState ->
            { model |
              state = rollForwardState,
              redoStack = (List.tail redoStack |> Maybe.withDefault []),
              undoStack = state :: undoStack
            } ! [ setFirebaseState { session = "1", watchers = rollForwardState.watchers } ]

    _ ->
      let
        (newState, stateFx) = updateState msg model.state
      in
        { model | state = newState } ! [ stateFx ]



episodesBySeason : State -> List Season
episodesBySeason state =
  let
    groupedEpisodes : Dict Int (List Episode)
    groupedEpisodes =
      List.foldr
        (\item dict ->
          Dict.update
            item.season
            (\list -> Just
              (case list of
                Just arr -> item :: arr
                Nothing -> [ item ]))
            dict
        )
        (Dict.empty)
        state.episodes
  in
    groupedEpisodes
      |> Dict.toList
      |> List.map (\tuple ->
          let (season, episodes) = tuple in
          { season = season, episodes = episodes }
        )


view model =
  div [ class "container"] [
    div [ class "row" ] [
      div [ class "col-md-12" ] [
        button
          [ class "btn btn-default"
          , onClick Undo
          , disabled (List.isEmpty model.undoStack) ] [
          text <| "undo (" ++ (toString <| List.length model.undoStack) ++ ")"
        ],
        button
          [ class "btn btn-default"
          , onClick Redo
          , disabled (List.isEmpty model.redoStack) ] [
          text <| "redo (" ++ (toString <| List.length model.redoStack) ++ ")"
        ]
      ]
    ],

    (renderSelectUser model.state),

    div [ class "row" ] [
      div [ class "col-md-12" ] [
        viewFromState model.state
      ]
    ]
  ]

viewFromState : State -> Html Msg
viewFromState state =
  let
    errorMsg =
      case state.error of
        Just err -> text "There was an error loading the episodes."
        Nothing -> text ""

    lastWatchedSeasons =
      state.watchers
        |> List.map (fst << .lastWatchedEpisode)

    lowestSeason : Int
    lowestSeason =
      lastWatchedSeasons
        |> List.minimum
        |> Maybe.withDefault 0

    highestSeason : Int
    highestSeason =
      lastWatchedSeasons
        |> List.maximum
        |> Maybe.withDefault 1


    bySeason = episodesBySeason state

    seasons = bySeason
      |> List.filter (.season >> (>=) highestSeason)
      |> List.map
        (renderSeason {
          watchers = state.watchers,
          lowest = lowestSeason,
          highest = highestSeason
        })
  in
    div [] [
      errorMsg
      , div [] seasons
    ]


renderSelectUser : State -> Html Msg
renderSelectUser { watchers, currentUserId } =
  div [ class "row" ] [
    div [ class "col-md-12" ] [
      div [ class "btn-group" ]
        (List.map (renderUser currentUserId) watchers)
    ]
  ]


renderUser : Maybe Int -> User -> Html Msg
renderUser currentUserId user =
  let
    isSelected =
      case currentUserId of
        Nothing -> False

        Just id -> user.id == id
  in
    button
      [ classList [
          ("btn", True)
        , ("btn-primary", isSelected)
        , ("btn-default", not isSelected) ]
      , onClick (SelectUser user.id)
      ] [ text user.name ]


renderSeason : { watchers : List User, lowest : Int, highest : Int } -> Season -> Html Msg
renderSeason { watchers, lowest, highest } seasonObj =
  let
    title : String
    title = "Season " ++ (toString seasonObj.season)

    total : String
    total = seasonObj |> .episodes |> List.length |> toString

    isBefore = lowest > seasonObj.season
    isAfter = highest < seasonObj.season

    shouldCollapse : Bool
    shouldCollapse =  isBefore || isAfter

    heading = div [] [
      h3 [] [
        text title
        , text ( " (" ++ total ++ ")" )
      ]
    ]

    collapseHtml : Html Msg
    collapseHtml = div [ class "text-muted "] [
      if isBefore then text "Everyone has watched this season!"
        else
          text "Not there yet!"
    ]

    episodes =
      (List.map (renderEpisode watchers) seasonObj.episodes)

    children = if shouldCollapse then [ collapseHtml ] else episodes
  in
    div []
      [
        heading
      , div [] children
      ]


renderEpisode : List User -> Episode -> Html Msg
renderEpisode watchers episode =
  div [ class "row row-hover" ] [
    div [ class "col-xs-8" ] [
      a [ href episode.url, target "_blank" ] [ text episode.name ]
    ],
    div [ class "col-xs-4" ]
      (List.map (renderWatcher episode) watchers)
  ]


renderWatcher : Episode -> User -> Html Msg
renderWatcher episode watcher =
  let
    (season, number) = watcher.lastWatchedEpisode

    watched = hasWatched episode watcher

    isLastWatched = episode.season == season && episode.number == number

    label = "label-success"
  in
    button
      [ classList
        [ ("watcher-btn btn btn-xs", True)
        , ("btn-success", watched)
        , ("btn-default not-watched", not watched) ]
      , style [ ("margin-right", "10px") ]
      , disabled (watched && not isLastWatched)
      , onClick <| Interact
          <| if isLastWatched then
            UnSelectEpisode episode watcher
              else SelectEpisode episode watcher
      ] [
        text watcher.name
      ]


hasWatched : Episode -> User -> Bool
hasWatched episode user =
  let
    (season, number) = user.lastWatchedEpisode
  in
    if episode.season < season then True else
      if episode.season == season && episode.number <= number then True else
        False


type alias FirebaseSession = {
  watchers : List User,
  showId : String
}

port setFirebaseState : { session: String, watchers: List User } -> Cmd msg
port firebaseSession : (FirebaseSession -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model =
  firebaseSession LoadSession
