module FreeLayout2 exposing
    ( ScreenClass(..)
    , LayoutState, LayoutConfig, WrappedConfig, WindowSize, windowSizeDecoder, init, update
    , bodyAttributes
    )

{-| `FreeLayout2` stands for 2 screen classes: Mobile and Desktop.

@docs ScreenClass


# Shared

@docs LayoutState, LayoutConfig, WrappedConfig, WindowSize, windowSizeDecoder, init, update


# Layout

@docs bodyAttributes

-}

import Element exposing (..)
import Json.Decode


{-| A screen class. Similar to `Element.DeviceClass` from `elm-ui`,
but narrowed down to support only 2 devices both in grid layout and in the application code.
Names differ from `Element.DeviceClass` to avoid import conflicts
when importing everything from both `Element` and `FreeLayout2`.
-}
type ScreenClass
    = MobileScreen
    | DesktopScreen


{-| Layout state. A value of this type contains everything needed to render a layout or any grid-aware element.

  - `window` – the current window size.
  - `screenClass` – the current screen class.
  - `config` – the layout configuration. Not to be changed during the app lifecycle, or even accessed directly.

-}
type alias LayoutState =
    { window : WindowSize
    , screenClass : ScreenClass
    , config : WrappedConfig
    }


{-| Layout configuration. Needs to be passed in the `init` function once per app.

  - `mobileScreen.minGridWidth` – Includes grid margins.
    The MobileScreen Figma layouts should use this width first.
    If the window width is less than this value, we display a horizontal scroll.
  - `desktopScreen.minGridWidth` – Includes grid margins.
    The DesktopScreen Figma layouts should use this width first.
    If the window width is equal to or greater than this value, the screen class is DesktopScreen.

-}
type alias LayoutConfig =
    { mobileScreen :
        { minGridWidth : Int
        }
    , desktopScreen :
        { minGridWidth : Int
        }
    }


{-| `LayoutConfig` is not meant to be accessed from the client code, so it's wrapped in this type to prevent direct access.
-}
type WrappedConfig
    = WrappedConfig LayoutConfig


{-| A helper to access the wrapped config.
-}
accessConfig : WrappedConfig -> LayoutConfig
accessConfig (WrappedConfig config) =
    config


{-| A window size object coming from Flags and constructed from the `Browser.Events.onResize` event.
-}
type alias WindowSize =
    { width : Int
    , height : Int
    }


{-| A decoder for the `WindowSize` type, for Flags.
-}
windowSizeDecoder : Json.Decode.Decoder WindowSize
windowSizeDecoder =
    Json.Decode.map2 WindowSize
        (Json.Decode.field "width" Json.Decode.int)
        (Json.Decode.field "height" Json.Decode.int)


{-| Initializes the layout state, which then needs to be stored in some sort of `Shared.Model`.
-}
init : LayoutConfig -> WindowSize -> LayoutState
init config window =
    let
        screenClass : ScreenClass
        screenClass =
            if window.width < config.desktopScreen.minGridWidth then
                MobileScreen

            else
                DesktopScreen
    in
    { window = window
    , screenClass = screenClass
    , config = WrappedConfig config
    }


{-| Updates the layout state. The previous state is passed from the `Shared.Model`.
-}
update : LayoutState -> WindowSize -> LayoutState
update { config } window =
    init (accessConfig config) window


{-| A helper to build the application `Layout`. See Readme for example usage.
-}
bodyAttributes : LayoutState -> List (Attribute msg)
bodyAttributes layout =
    [ width (fill |> minimum (accessConfig layout.config).mobileScreen.minGridWidth) ]
