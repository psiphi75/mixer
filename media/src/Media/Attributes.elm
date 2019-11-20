module Media.Attributes
    exposing
        ( volume
        , muted
        , loop
        , controls
        , autoplay
        , playbackRate
        , crossOrigin
        , anonymous
        , useCredentials
        , playsInline
        , poster
        , preloadAuto
        , preloadMetadata
        , preloadNone
        , mode
        , label
        )

{-|


### Attributes

These are provided to have all the properties and attributes for media
elements in one place. Attributes that are included in Html.Attributes, such
as loop, controls, autoplay are implemented better here, and several new ones are added.

@docs loop, controls, autoplay, muted, volume, playbackRate, crossOrigin, anonymous, useCredentials, playsinline, showTrack, hideTrack, disableTrack, label, controlList

-}

import Html.Attributes as Attrs exposing (property, attribute)
import Html exposing (Attribute)
import Json.Encode exposing (string, bool, float, int, list)
import Media.State exposing (TextTrack, TextTrackMode(..), TextTrackKind(..))
import Internal.Helpers


{-| -}
controls : Bool -> Attribute msg
controls opt =
    property "controls" (bool opt)


{-| -}
muted : Bool -> Attribute msg
muted opt =
    property "muted" (bool opt)


{-| -}
volume : Float -> Attribute msg
volume vol =
    property "volume" (float vol)


{-| -}
loop : Bool -> Attribute msg
loop opt =
    property "loop" (bool opt)


{-| -}
autoplay : Bool -> Attribute msg
autoplay opt =
    property "autoplay" (bool opt)


{-| -}
playbackRate : Float -> Attribute msg
playbackRate rate =
    property "playbackRate" (float rate)


type CORS
    = Anonymous
    | UseCredentials


{-| -}
crossOrigin : CORS -> Attribute msg
crossOrigin policy =
    let
        policyString =
            case policy of
                Anonymous ->
                    "anonymous"

                UseCredentials ->
                    "use-credentials"
    in
        property "crossOrigin" (string policyString)


{-| -}
anonymous : CORS
anonymous =
    Anonymous


{-| -}
useCredentials : CORS
useCredentials =
    UseCredentials


{-| -}
playsInline : Bool -> Attribute msg
playsInline opt =
    property "playsInline" (bool opt)


{-| -}
poster : String -> Attribute msg
poster url =
    property "poster" (string url)


{-| -}
preloadAuto : Attribute msg
preloadAuto =
    property "preload" (string "auto")


{-| -}
preloadMetadata : Attribute msg
preloadMetadata =
    property "preload" (string "metadata")


{-| -}
preloadNone : Attribute msg
preloadNone =
    property "preload" (string "none")


{-| -}
mode : TextTrackMode -> Attribute msg
mode md =
    property "mode" (string <| Internal.Helpers.textTrackModeToString md)


{-| -}
label : String -> Attribute msg
label lbl =
    property "label" (string lbl)


{-| -}
kind : TextTrackKind -> Attribute msg
kind knd =
    let
        kindToString =
            case knd of
                Subtitles ->
                    "subtitles"

                Descriptions ->
                    "description"

                Metadata ->
                    "metadata"

                Captions ->
                    "captions"

                Chapters ->
                    "chapters"

                Other str ->
                    str

                _ ->
                    ""
    in
        property "kind" <| string kindToString
