port module Main exposing (Model, Msg(..), init, main, toJs, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (Error(..))
import Json.Decode as Decode
import List as L



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { chores : List Chore
    , people : List String
    }


type alias Chore =
    { name : String
    , freq : Frequency
    }


type Frequency
    = Weekly
    | Monthly


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
      , people = [ "Adam", "Ron", "Natalie", "Jordan" ]
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )



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
        [ header []
            [ h1 [] [ text "Housemate chore roster" ]
            ]
        , table [ class "pure-table pure-table-bordered" ]
            [ thead [] [ headerRow ]
            , tbody [] (L.map choreToRow model.chores)
            ]
        ]


choreToRow : Chore -> Html Msg
choreToRow chore =
    tr []
        [ textTD chore.name
        , textTD (toString chore.freq)
        , textTD "Adam"
        ]



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
