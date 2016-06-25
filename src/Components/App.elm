module Components.App exposing (init, view, update, subscriptions)

import Html exposing (Html, div, span, button, text, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, style)
import Http
import Task exposing (..)
import Json.Decode as Json exposing ((:=))
import Dict exposing (Dict)
import Set exposing (Set)


type alias Episode = {
  id : Int,
  name : String,
  season : Int,
  number : Int
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

type alias Model = {
  episodes : List Episode,
  error : Maybe Http.Error,
  watchers : List User
}


type Msg =
  Load (List Episode)
  | Error Http.Error


initUser : String -> Int -> (Int, Int) -> User
initUser name id lastWatchedEpisode =
  User name id lastWatchedEpisode


loadEpisodes : Task Http.Error (List Episode)
loadEpisodes =
  Http.get decodeEpisodes "http://api.tvmaze.com/shows/167/episodes"


decodeEpisodes : Json.Decoder (List Episode)
decodeEpisodes =
  let
    episode =
      Json.object4 Episode
        ("id" := Json.int)
        ("name" := Json.string)
        ("season" := Json.int)
        ("number" := Json.int)
  in
    Json.list episode


init : (Model, Cmd Msg)
init =
  { episodes = []
  , error = Nothing
  , watchers = [ initUser "Sarah" 1 (3, 5), initUser "James" 2 (3, 9) ]
  } ! [
    Task.perform Error Load loadEpisodes
  ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Load episodes ->
      { model | episodes = episodes } ! []

    Error err ->
      { model | error = Just err } ! []


episodesBySeason : Model -> List Season
episodesBySeason model =
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
        model.episodes
  in
    groupedEpisodes
      |> Dict.toList
      |> List.map (\tuple ->
          let (season, episodes) = tuple in
          { season = season, episodes = episodes }
        )


view : Model -> Html Msg
view model =
  let
    errorMsg =
      case model.error of
        Just err -> text "There was an error loading the episodes."
        Nothing -> text ""

    lastWatchedSeasons =
      model.watchers
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
        |> Maybe.withDefault 0


    bySeason = episodesBySeason model

    seasons =
      List.map
        (renderSeason {
          watchers = model.watchers,
          lowest = lowestSeason,
          highest = highestSeason
        }) bySeason
  in
    div [ class "container" ] [
      div [ class "row" ] [
        div [ class "col-md-8 col-md-offset-2" ] [
          errorMsg
          , div [] seasons
        ]
      ]
    ]


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
  div [ class "row" ] [
    div [ class "col-xs-8" ] [
      text episode.name,
      text <| toString episode.number
    ],
    div [ class "col-xs-4" ]
      (List.map (renderWatcher episode) watchers)
  ]


renderWatcher : Episode -> User -> Html Msg
renderWatcher episode watcher =
  let
    watched = hasWatched episode watcher

    label = "label-success"
  in
    if watched then
      span
        [ class ("label " ++ label)
        , style [ ("margin-right", "10px") ]
        ] [
          text watcher.name
        ]
      else
        span [] []

hasWatched : Episode -> User -> Bool
hasWatched episode user =
  let
    (season, number) = user.lastWatchedEpisode
  in
    if episode.season < season then True else
      if episode.season == season && episode.number <= number then True else
        False

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
