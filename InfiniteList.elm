module InfiniteList exposing (..)

import Lazy exposing (..)

type InfiniteList a 
    = Cons a (Lazy (InfiniteList a))

map : (a -> b) -> InfiniteList a -> InfiniteList b
map f (Cons x xs) = Cons (f x) (Lazy.map (map f) xs)

head : InfiniteList a -> a
head (Cons x xs) = x

tail : InfiniteList a -> InfiniteList a
tail (Cons x xs) = Lazy.force xs


countFrom : Int -> InfiniteList Int
countFrom n = Cons n (lazy (\() -> countFrom (n+1)))


get : Int -> InfiniteList a -> a
get n l =
    case n of
        0 -> head l
        _ -> get (n-1) (tail l)
