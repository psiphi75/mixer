module Main exposing (Model, Msg(..), init, main, update, view)

import AudioPlayer
import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, h1, p, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http



-- GLOBALS


siteTitle : String
siteTitle =
    "Mix"



-- MODEL


type alias PlayerGroupModel =
    { original : AudioPlayer.Model
    , mix1 : AudioPlayer.Model
    , mix2 : AudioPlayer.Model
    }


type alias Model =
    { sample : PlayerGroupModel
    , userGroup : Maybe PlayerGroupModel
    , uploadState : UploadState
    }


type UploadState
    = Waiting
    | Uploading Float
    | UploadDone
    | Fail String


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { sample = newPlayerGroup "sample"
            , userGroup = Nothing
            , uploadState = Waiting
            }
    in
    ( model, Cmd.none )


newPlayerGroup : String -> PlayerGroupModel
newPlayerGroup tag =
    { original = AudioPlayer.new (tag ++ "-original") (tag ++ "/original.mp3")
    , mix1 = AudioPlayer.new (tag ++ "-mix1") (tag ++ "/mix1.mp3")
    , mix2 = AudioPlayer.new (tag ++ "-mix2") (tag ++ "/mix2.mp3")
    }



-- UPDATE


type Msg
    = SampleAudioMsg PlayerGroupMsg


type PlayerGroupMsg
    = Original AudioPlayer.Msg
    | Mix1 AudioPlayer.Msg
    | Mix2 AudioPlayer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SampleAudioMsg subMsg ->
            let
                ( updatedPlayerGroupModel, updatedCmd ) =
                    updatePlayerGroup subMsg model.sample
            in
            ( { model | sample = updatedPlayerGroupModel }
            , Cmd.map SampleAudioMsg updatedCmd
            )


updatePlayerGroup : PlayerGroupMsg -> PlayerGroupModel -> ( PlayerGroupModel, Cmd PlayerGroupMsg )
updatePlayerGroup msg model =
    case msg of
        Original subMsg ->
            let
                ( updatedAudioPlayerModel, updatedCmd ) =
                    AudioPlayer.update subMsg model.original
            in
            ( { model | original = updatedAudioPlayerModel }
            , Cmd.map Original updatedCmd
            )

        Mix1 subMsg ->
            let
                ( updatedAudioPlayerModel, updatedCmd ) =
                    AudioPlayer.update subMsg model.mix1
            in
            ( { model | mix1 = updatedAudioPlayerModel }
            , Cmd.map Mix1 updatedCmd
            )

        Mix2 subMsg ->
            let
                ( updatedAudioPlayerModel, updatedCmd ) =
                    AudioPlayer.update subMsg model.mix2
            in
            ( { model | mix2 = updatedAudioPlayerModel }
            , Cmd.map Mix2 updatedCmd
            )


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader
        , viewDemo model.sample
        , viewFooter
        ]


viewHeader : Html Msg
viewHeader =
    div [ class "banner" ]
        [ h1 [ class "banner-head" ] [ text siteTitle ]
        ]


viewDemo : PlayerGroupModel -> Html Msg
viewDemo sample =
    viewPlayerGroup sample


viewPlayerGroup : PlayerGroupModel -> Html Msg
viewPlayerGroup pg =
    div [ class "pure-g" ]
        [ viewPlayer pg.original Original
            |> Html.map SampleAudioMsg
        , viewPlayer pg.mix1 Mix1
            |> Html.map SampleAudioMsg
        , viewPlayer pg.mix2 Mix2
            |> Html.map SampleAudioMsg
        ]


viewPlayer : AudioPlayer.Model -> (AudioPlayer.Msg -> PlayerGroupMsg) -> Html PlayerGroupMsg
viewPlayer player msg =
    div [ class "pure-u-1-3" ]
        [ p []
            [ text player.tag ]
        , div []
            [ AudioPlayer.view player
                |> Html.map msg
            ]
        ]


viewFooter : Html Msg
viewFooter =
    div [] []



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
