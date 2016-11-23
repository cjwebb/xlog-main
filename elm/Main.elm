module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    Int


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ nav []
            [ div [ class "container" ]
                [ div [ class "navbar-header" ]
                    -- todo: burger menu
                    [ a [ class "navbar-brand" ] [ text "xlog" ] ]
                ]
            ]
        , div [ class "container" ]
            [ div [ class "starter-template" ]
                [ h1 [] [ text "xlog" ]
                ]
            ]
        ]
