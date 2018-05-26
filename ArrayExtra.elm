module ArrayExtra exposing (..)
{-| This module exists because core's Array is broken and elm-community/array-extra is only for core's Array, and does not contain `concat`. -}
import Array.Hamt exposing (..)

concat : Array (Array a) -> Array a
concat = foldr append empty

concatMap : (a -> Array b) -> Array a -> Array b
concatMap f a = concat (map f a)



-- Everything after this point is copied from elm-community/array-extra.

{-| Unsafe version of get, don't use this unless you know what you're doing!
-}
getUnsafe : Int -> Array a -> a
getUnsafe n xs =
    case get n xs of
        Just x ->
            x

        Nothing ->
            Debug.crash ("Index " ++ Basics.toString n ++ " of Array with length " ++ Basics.toString (length xs) ++ " is not reachable.")



{-| Apply an array of functions to an array of values.
-}
apply : Array (a -> b) -> Array a -> Array b
apply fs xs =
    let
        l =
            min (length fs) (length xs)

        fs_ =
            slice 0 l fs
    in
        indexedMap (\n f -> f (getUnsafe n xs)) fs_


{-| Combine two arrays, combining them with the given function.
If one array is longer, the extra elements are dropped.
    map2 (+) [1,2,3] [1,2,3,4] == [2,4,6]
    map2 (,) [1,2,3] ['a','b'] == [ (1,'a'), (2,'b') ]
    pairs : Array a -> Array b -> Array (a,b)
    pairs lefts rights =
        map2 (,) lefts rights
-}
map2 : (a -> b -> result) -> Array a -> Array b -> Array result
map2 f ws =
    apply (map f ws)


{-| -}
map3 : (a -> b -> c -> result) -> Array a -> Array b -> Array c -> Array result
map3 f ws xs =
    apply (map2 f ws xs)


{-| -}
map4 : (a -> b -> c -> d -> result) -> Array a -> Array b -> Array c -> Array d -> Array result
map4 f ws xs ys =
    apply (map3 f ws xs ys)
