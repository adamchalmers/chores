port module Main exposing (Model, Msg(..), init, main, personFor, toJs, update, view)

import Array as A exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode
import List as L
import Task exposing (Task)
import Time exposing (millisToPosix, toDay, utc)



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { chores : List Chore
    , timestamp : Int
    }


type alias Chore =
    { name : String
    , freq : Frequency
    }


type Frequency
    = Weekly
    | Monthly


allPeople =
    [ "Adam", "Ron", "Natalie", "Jordan" ]


init : Int -> ( Model, Cmd Msg )
init flags =
    ( { chores =
            [ { name = "Vacuum stairs", freq = Weekly }
            , { name = "Vacuum landing", freq = Weekly }
            , { name = "Stove", freq = Weekly }
            , { name = "Sink", freq = Weekly }
            , { name = "Mop downstairs", freq = Weekly }
            , { name = "Wipe down countertops", freq = Weekly }
            , { name = "Wash glass table", freq = Weekly }
            , { name = "Wash glass doors/front door window", freq = Weekly }
            , { name = "Wash fridge shelves", freq = Monthly }
            , { name = "Wipe fridge outside", freq = Monthly }
            , { name = "Clean microwave inside/outside", freq = Monthly }
            , { name = "Dusting/baseboards", freq = Monthly }
            , { name = "Clutter check (put away if you know where it is, if you don’t know whose it is, just send a photo to the group)", freq = Monthly }
            ]
      , timestamp = 0
      }
    , Task.perform UpdateTime millisSinceEpoch
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Noop
    | UpdateTime Int


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )

        UpdateTime t ->
            ( { model | timestamp = t }, Cmd.none )


millisecondsInAWeek =
    1000 * 60 * 60 * 24 * 7


{-| Assume a month is 4 weeks
-}
millisecondsInAMonth =
    millisecondsInAWeek * 4


millisSinceEpoch : Task x Int
millisSinceEpoch =
    Task.map Time.posixToMillis Time.now



-- ---------------------------
-- VIEW
-- ---------------------------


textTD s =
    td [] [ text s ]


toString chore =
    case chore of
        Weekly ->
            "Weekly"

        Monthly ->
            "Monthly"


view : Model -> Html Msg
view model =
    let
        headerRow =
            [ "Chore", "Frequency", "Who" ]
                |> L.map (\s -> th [] [ text s ])
                |> tr []
    in
    div [ class "container" ]
        [ h1 [] [ text "Housemate chore roster" ]
        , h2 [] [ text <| describeDay model ]
        , table [ class "pure-table pure-table-bordered" ]
            [ thead [] [ headerRow ]
            , tbody [] (L.indexedMap choreToRow model.chores)
            ]
        , p [] [ text ("Unix timestamp " ++ String.fromInt model.timestamp) ]
        ]


describeDay model =
    "Day " ++ String.fromInt (dayOfMonth model)


dayOfMonth model =
    modBy 28 <| (model.timestamp // millisecondsInAWeek)


choreToRow : Int -> Chore -> Html Msg
choreToRow i chore =
    tr []
        [ textTD chore.name
        , textTD (toString chore.freq)
        , textTD "Adam"
        ]



-- Given the day of the month (0-27) and a chore number, choose a person.


personFor : Int -> Int -> Array String -> String
personFor i day people =
    let
        x =
            day // 7

        n =
            A.length people

        index =
            modBy n x
    in
    case A.get index people of
        Just p ->
            p

        Nothing ->
            "???"



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
