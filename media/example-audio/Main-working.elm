port module Main exposing (Model, Msg(..), init, main, outbound, update, view)

import Browser
import Html exposing (button, div, p, text, track)
import Html.Attributes exposing (id, kind, src, srclang)
import Html.Events exposing (onClick)
import Html.Keyed exposing (node)
import Media exposing (PortMsg, audio, load, mute, newAudio, pause, play, seek)
import Media.Attributes exposing (anonymous, autoplay, controls, crossOrigin, label, mode, playsInline)
import Media.Events
import Media.Source exposing (mediaCapture, source)
import Media.State exposing (PlaybackStatus(..), currentTime, duration, playbackStatus, played)


port outbound : PortMsg -> Cmd msg


type alias Model =
    { state : Media.State
    }


type Msg
    = NoOp
    | Play
    | Pause
    | Seek Float
    | MediaStateUpdate Media.State


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { state = newAudio "myAudio" }
    in
    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    let
        playPauseButton =
            case playbackStatus model.state of
                Playing ->
                    button [ onClick Pause ] [ text "Pause" ]

                Paused ->
                    button [ onClick Play ] [ text "Play" ]

                _ ->
                    button [ onClick (Seek 0) ] [ text "Other" ]

        audioElement =
            ( "audio"
            , audio
                model.state
                (Media.Events.allEvents MediaStateUpdate
                    ++ [ playsInline True, controls False, crossOrigin anonymous ]
                )
                [ source "sample.mp3" []
                ]
            )

        mediaInfo =
            [ p [] [ text ("current: " ++ (String.fromFloat <| currentTime model.state)) ]
            , p [] [ text ("duration: " ++ (String.fromFloat <| duration model.state)) ]
            ]
    in
    { title = "Elm Media Example"
    , body =
        [ div []
            [ node "div" [] [ audioElement ]
            , p []
                [ playPauseButton
                , button [ onClick <| skip5s model.state ] [ text ">>" ]
                ]
            , div [] mediaInfo
            ]
        ]
    }


skip5s : Media.State -> Msg
skip5s audioState =
    Seek <| currentTime audioState + 5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "model" model
    in
    case msg of
        Play ->
            ( model, play model.state outbound )

        Pause ->
            ( model, pause model.state outbound )

        Seek time ->
            ( model, seek model.state time outbound )

        MediaStateUpdate state ->
            ( { model | state = state }, Cmd.none )

        _ ->
            ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
