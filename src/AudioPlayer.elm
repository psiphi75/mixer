port module AudioPlayer exposing (Model, Msg(..), new, outbound, update, view)

import Html exposing (Html, button, div, p, text)
import Html.Events exposing (onClick)
import Html.Keyed exposing (node)
import Media exposing (PortMsg, audio, newAudio, pause, play, seek)
import Media.Attributes exposing (anonymous, controls, crossOrigin, playsInline)
import Media.Events
import Media.Source exposing (source)
import Media.State exposing (PlaybackStatus(..), currentTime, duration, playbackStatus)


port outbound : PortMsg -> Cmd msg


type alias Model =
    { tag : String
    , url : String
    , audio : Media.State
    }


type Msg
    = NoOp
    | Play
    | Pause
    | Seek Float
    | MediaStateUpdate Media.State


new : String -> String -> Model
new tag url =
    { tag = tag
    , audio = newAudio tag
    , url = url
    }


view : Model -> Html Msg
view model =
    let
        player =
            case playbackStatus model.audio of
                Loading ->
                    viewLoading

                _ ->
                    viewPlayer model

        audioElement =
            ( "audio"
            , audio
                model.audio
                (Media.Events.allEvents MediaStateUpdate
                    ++ [ playsInline True, controls False, crossOrigin anonymous ]
                )
                [ source model.url []
                ]
            )
    in
    div []
        [ node "div" [] [ audioElement ]
        , player
        ]


viewLoading : Html Msg
viewLoading =
    div []
        [ p [] [ text "Loading" ]
        ]


viewPlayer : Model -> Html Msg
viewPlayer model =
    let
        playPauseButton =
            case playbackStatus model.audio of
                Playing ->
                    button [ onClick Pause ] [ text "Pause" ]

                Paused ->
                    button [ onClick Play ] [ text "Play" ]

                _ ->
                    button [ onClick (Seek 0) ] [ text "Other" ]

        mediaInfo =
            [ p [] [ text ("current: " ++ (String.fromFloat <| currentTime model.audio)) ]
            , p [] [ text ("duration: " ++ (String.fromFloat <| duration model.audio)) ]
            ]
    in
    div []
        [ p []
            [ playPauseButton
            , button [ onClick <| skip5s model.audio ] [ text ">>" ]
            ]
        , div [] mediaInfo
        ]


skip5s : Media.State -> Msg
skip5s audioState =
    Seek <| currentTime audioState + 5


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "model" model

        _ =
            Debug.log "msg" msg
    in
    case msg of
        Play ->
            ( model, play model.audio outbound )

        Pause ->
            ( model, pause model.audio outbound )

        Seek time ->
            ( model, seek model.audio time outbound )

        MediaStateUpdate audio ->
            ( { model | audio = audio }, Cmd.none )

        _ ->
            ( model, Cmd.none )
