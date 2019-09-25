port module Main exposing (Frequency(..), Model, Msg(..), init, main, personFor, toJs, update, view)

import Array as A exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode as Decode
import List as L
import String exposing (startsWith, toLower)
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
    , day : Int
    , filter : String
    }


type alias Chore =
    { name : String
    , freq : Frequency
    }


type Frequency
    = Weekly
    | Monthly


daysPer : Frequency -> Int
daysPer f =
    case f of
        Weekly ->
            7

        Monthly ->
            28


allPeople =
    A.fromList [ "Adam", "Ron", "Natalie", "Jordan" ]


init : () -> ( Model, Cmd Msg )
init _ =
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
      , day = 0
      , filter = ""
      }
    , Task.perform UpdateDay millisSinceEpoch
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Noop
    | Inc
    | Dec
    | UpdateDay Int
    | UpdateFilter String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )

        UpdateDay t ->
            ( { model | day = t }, Cmd.none )

        UpdateFilter s ->
            ( { model | filter = s }, Cmd.none )

        Inc ->
            ( { model | day = model.day + 1 }, Cmd.none )

        Dec ->
            ( { model | day = model.day - 1 }, Cmd.none )


millisecondsInAWeek =
    1000 * 60 * 60 * 24 * 7


{-| Assume a month is 4 weeks
-}
millisecondsInAMonth =
    millisecondsInAWeek * 4


millisSinceEpoch : Task x Int
millisSinceEpoch =
    Task.map (dayOfMonth << Time.posixToMillis) Time.now



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

        bodyRows =
            model.chores
                |> L.indexedMap (choreToRow <| model.day)
                |> L.filter (\( _, person ) -> startsWith (toLower model.filter) (toLower person))
                |> L.map (\( row, person ) -> row)
    in
    div [ class "container" ]
        [ h1 [ style "text-align" "center" ] [ text <| "Housemate chore roster" ]
        , text "Filter by person: "
        , input [ type_ "input", onInput UpdateFilter ] []
        , hr [] []
        , br [] []
        , table [ class "pure-table pure-table-bordered", style "width" "100%" ]
            [ col [ style "width" "60%" ] []
            , col [ style "width" "20%" ] []
            , col [ style "width" "20%" ] []
            , thead [] [ headerRow ]
            , tbody [] bodyRows
            ]
        , br [] []
        , text <| "Showing day " ++ describeDay model ++ " of 28. Change date: "
        , button [ onClick Inc, class "pure-button" ] [ text "+" ]
        , button [ onClick Dec, class "pure-button" ] [ text "-" ]
        ]


describeDay model =
    String.fromInt <| 1 + modBy 28 model.day


dayOfMonth millisecondsSinceEpoch =
    modBy 28 <| (millisecondsSinceEpoch // millisecondsInAWeek)


choreToRow : Int -> Int -> Chore -> ( Html Msg, String )
choreToRow day i chore =
    let
        person =
            personFor chore.freq day i allPeople

        row =
            tr []
                [ textTD chore.name
                , textTD (toString chore.freq)
                , textTD person
                ]
    in
    ( row, person )



-- Given the day of the month (0-27) and a chore number, choose a person.


personFor : Frequency -> Int -> Int -> Array String -> String
personFor freq day i people =
    let
        x =
            day // daysPer freq

        index =
            (x + i) |> modBy (A.length people)
    in
    case A.get index people of
        Just p ->
            p

        Nothing ->
            "???"



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program () Model Msg
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
