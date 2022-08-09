module Data.Located exposing
    ( Located, Region, Position
    , located
    , map, replaceWith, toValue
    , getRegion
    )

{-| A `Located` is a value with row-column region location data.


# Types

@docs Located, Region, Position


# Create

@docs located


# Transform

@docs map, replaceWith, toValue


# Query

@docs getRegion

-}


{-| A `Located` is a value with a location.
-}
type Located a
    = Located Region a


{-| -}
type alias Region =
    { start : Position
    , end : Position
    }


{-| -}
type alias Position =
    { row : Int
    , col : Int
    }


{-| Create a located value.
-}
located : Region -> a -> Located a
located =
    Located


{-| Apply a function to a located value.
-}
map : (a -> b) -> Located a -> Located b
map fn (Located region value) =
    Located region (fn value)


{-| Replace the located value with another.
-}
replaceWith : b -> Located a -> Located b
replaceWith value (Located region _) =
    Located region value


{-| Return the value.
-}
toValue : Located a -> a
toValue (Located _ value) =
    value


{-| Return the location info.
-}
getRegion : Located a -> Region
getRegion (Located region _) =
    region
