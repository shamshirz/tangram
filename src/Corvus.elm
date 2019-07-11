module Main exposing (Model, Msg(..), Palette, corvus, init, main, palette, tangram, update, view)

import Animation exposing (px)
import Browser
import Html exposing (Html, div, h1)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


type alias Model =
    { styles : List Animation.State
    , index : Int
    , currentShape : Shape
    }


type Shape
    = Constellation
    | Tangram


type Msg
    = EverybodySwitch
    | Animate Animation.Msg


type alias Color =
    { red : Int, blue : Int, green : Int, alpha : Float }


rgb r g b =
    { red = r, blue = b, green = g, alpha = 1 }


type alias Palette =
    { orange : Color
    , green : Color
    , lavender : Color
    , blue : Color
    }


palette : Palette
palette =
    { orange = rgb 240 173 0
    , green = rgb 127 209 59
    , lavender = rgb 90 99 120
    , blue = rgb 96 181 204
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        EverybodySwitch ->
            let
                ( newStyleList, newShape ) =
                    case model.currentShape of
                        Constellation ->
                            ( tangram, Tangram )

                        Tangram ->
                            ( corvus, Constellation )

                wrappedIndex =
                    if List.length model.styles < model.index then
                        model.index - List.length model.styles

                    else
                        model.index

                newStyles =
                    List.drop wrappedIndex newStyleList ++ List.take wrappedIndex newStyleList
            in
            ( { model
                | index = wrappedIndex + 1
                , currentShape = newShape
                , styles =
                    List.map3
                        (\i style newStyle ->
                            Animation.interrupt
                                [ Animation.wait (Time.millisToPosix (i * 50))
                                , Animation.to newStyle
                                ]
                                style
                        )
                        (List.range 0 (List.length model.styles))
                        model.styles
                        newStyles
              }
            , Cmd.none
            )

        Animate time ->
            ( { model
                | styles = List.map (Animation.update time) model.styles
              }
            , Cmd.none
            )


{-| The Raven has 7 distinct polygons (2 less than actual logo)
Since we want to translate smoothly, it's imporant that the image we are coming from also has 7 images
-}
corvus : List (List Animation.Property)
corvus =
    [ -- 1. Beak
      [ Animation.points
            [ ( 0, 17.316 )
            , ( 99.681, 49.061 )
            , ( 134.186, 0 )
            ]
      , Animation.fill palette.green
      ]

    -- 2. Chest
    , [ Animation.points
            [ ( 93.5, 241.745 )
            , ( 203.465, 344.538 )
            , ( 260.087, 357.275 )
            , ( 172.46, 204.694 )
            ]
      , Animation.fill palette.orange
      ]

    -- 3. Neck?
    , [ Animation.points
            [ ( 139.322, 4.397 )
            , ( 104.295, 54.201 )
            , ( 91.327, 235.316 )
            , ( 170.114, 198.346 )
            ]
      , Animation.fill palette.blue
      ]

    -- 4. Core
    , [ Animation.points
            [ ( 245.818, 131.749 )
            , ( 332.058, 294.554 )
            , ( 510.304, 392.559 )
            , ( 405.588, 256.698 )
            ]
      , Animation.fill palette.lavender
      ]

    -- 5. Tail?
    , [ Animation.points
            [ ( 600, 450 )
            , ( 330.196, 301.227 )
            , ( 272.465, 361.239 )
            , ( 456.368, 450 )
            ]
      , Animation.fill palette.blue
      ]

    -- 6. Belly?
    , [ Animation.points
            [ ( 325.448, 296.448 )
            , ( 234.943, 125.59 )
            , ( 177.862, 200.588 )
            , ( 267.505, 356.68 )
            ]
      , Animation.fill palette.green
      ]

    -- 7. Back
    , [ Animation.points
            [ ( 231.791, 118.619 )
            , ( 212.372, 42.825 )
            , ( 145.802, 2.333 )
            , ( 175.918, 192.027 )
            ]
      , Animation.fill palette.orange
      ]
    ]


tangram : List (List Animation.Property)
tangram =
    [ [ Animation.points
            [ ( 0, 20 )
            , ( 280, 300 )
            , ( 0, 580 )
            ]
      , Animation.fill palette.lavender

      -- Left triangle
      ]

    -- 2. Bottom triangle
    , [ Animation.points
            [ ( 20, 600 )
            , ( 300, 320 )
            , ( 580, 600 )
            ]
      , Animation.fill palette.blue
      ]

    -- 3. Top right triangle
    , [ Animation.points
            [ ( 320, 0 )
            , ( 600, 0 )
            , ( 600, 280 )
            ]
      , Animation.fill palette.blue
      ]

    -- 4. Rhombus
    , [ Animation.points
            [ ( 20, 0 )
            , ( 280, 0 )
            , ( 402, 122 )
            , ( 142, 122 )
            ]
      , Animation.fill palette.green
      ]

    -- 5. Middle Triangle
    , [ Animation.points
            [ ( 170, 150 )
            , ( 430, 150 )
            , ( 300, 280 )
            ]
      , Animation.fill palette.orange
      ]

    -- 6. Square
    , [ Animation.points
            [ ( 320, 300 )
            , ( 450, 170 )
            , ( 580, 300 )
            , ( 450, 430 )
            ]
      , Animation.fill palette.green
      ]

    -- 7. Bottom right triangle
    , [ Animation.points
            [ ( 470, 450 )
            , ( 600, 320 )
            , ( 600, 580 )
            ]
      , Animation.fill palette.orange
      ]
    ]


view : Model -> Html Msg
view model =
    div
        [ onClick EverybodySwitch
        , Attr.style "margin" "200px auto"
        , Attr.style "width" "500px"
        , Attr.style "height" "500px"
        , Attr.style "cursor" "pointer"
        ]
        [ h1 [] [ text "Click to morph!" ]
        , svg
            [ version "1.1"
            , x "0"
            , y "0"
            , viewBox "0 0 600 600"
            ]
          <|
            [ Svg.g []
                (List.map (\poly -> polygon (Animation.render poly) []) model.styles)
            ]
        ]


init : ( Model, Cmd Msg )
init =
    ( { styles = List.map Animation.style tangram
      , index = 1
      , currentShape = Tangram
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions =
            \model ->
                Animation.subscription
                    Animate
                    model.styles
        }
