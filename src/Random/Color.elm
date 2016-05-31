module Random.Color exposing (..)

{-| Extra randomized functions on colors.

# Random Colors by Components
@docs rgb, rgba, hsl, hsla

# Random Colors by Shade
@docs greyscale, grayscale, red, green, blue

-}

import Color exposing (Color)
import Random exposing (Generator, map, map3, map4, int, float)


{-| Generate a random non-transparent color by random RGB values.
-}
rgb : Generator Color
rgb =
    map3 Color.rgb (int 0 255) (int 0 255) (int 0 255)


{-| Generate a random transparent color by random RGBA values.
-}
rgba : Generator Color
rgba =
    map4 Color.rgba (int 0 255) (int 0 255) (int 0 255) (float 0 1)


{-| Generate a random non-transparent color by random HSL values.
-}
hsl : Generator Color
hsl =
    map3 Color.hsl (map degrees (float 0 360)) (float 0 1) (float 0 1)


{-| Generate a random transparent color by random HSLA values.
-}
hsla : Generator Color
hsla =
    map4 Color.hsla (map degrees (float 0 360)) (float 0 1) (float 0 1) (float 0 1)


{-| Generate a random shade of grey
-}
greyscale : Generator Color
greyscale =
    map Color.greyscale (float 0 1)


{-| Alias for greyscale.
-}
grayscale : Generator Color
grayscale =
    greyscale


{-| Generate a random shade of red.
-}
red : Generator Color
red =
    map (\red -> Color.rgb red 0 0) (int 0 255)


{-| Generate a random shade of green.
-}
green : Generator Color
green =
    map (\green -> Color.rgb 0 green 0) (int 0 255)


{-| Generate a random shade of blue.
-}
blue : Generator Color
blue =
    map (\blue -> Color.rgb 0 0 blue) (int 0 255)
