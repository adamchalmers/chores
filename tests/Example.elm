module Example exposing (unitTest)

import Array as A
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (tag, text)


{-| See <https://github.com/elm-community/elm-test>
-}
unitTest : Test
unitTest =
    describe "simple unit test"
        [ test "personFor week 1" <|
            \() ->
                personFor Weekly 0 0 people
                    |> Expect.equal "a"
        , test "personFor week 1 chore 2" <|
            \() ->
                personFor Weekly 0 1 people
                    |> Expect.equal "b"
        , test "personFor week 2" <|
            \() ->
                personFor Weekly 7 0 people
                    |> Expect.equal "b"
        , test "personFor week 3" <|
            \() ->
                personFor Weekly 14 0 people
                    |> Expect.equal "c"
        , test "personFor week 4" <|
            \() ->
                personFor Weekly 21 0 people
                    |> Expect.equal "d"
        , test "personFor week 5" <|
            \() ->
                personFor Weekly 28 0 people
                    |> Expect.equal "a"
        , test "personFor week 6" <|
            \() ->
                personFor Weekly 35 0 people
                    |> Expect.equal "b"
        ]


people =
    A.fromList [ "a", "b", "c", "d" ]
