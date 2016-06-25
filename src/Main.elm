import Components.App exposing (init, update, view, subscriptions)
import Html.App exposing (program)

main =
  program { init = init, update = update, view = view, subscriptions = subscriptions }
