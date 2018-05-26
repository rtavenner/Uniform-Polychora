module Symmetry exposing 
    ( Edge, Symmetry, runSymmetry
    , simplicial, tesseractic, snub24sym, regular24sym, regular120sym
    , grandAntiprismSym
    , tetrahedralPrismSym, snubCubicPrismSym, cubicPrismSym, snubDodecahedralPrismSym, dodecahedralPrismSym
    , duoprismSym, antiprismPrismSym)

import Matrix4 exposing (Float4x4,mulVector)
import Vector4 exposing (..)
import AllDict as Dict exposing (AllDict)
import Array.Hamt as Array exposing (Array)

type alias Edge = (Float4, Float4)
type alias Symmetry = List (List Float4x4)

runSymmetry : Symmetry -> Edge -> Array Edge
runSymmetry sym e =
    run sym e
    |> Dict.foldl (\k () -> Array.push k) Array.empty

run : Symmetry -> Edge -> AllDict Edge () (Vec4 Int, Vec4 Int) 
run sym (x,y) =
    case sym of
        [] -> Dict.singleton sortFn (x,y) ()
        first :: rest ->
            List.foldl 
                (\m acc -> Dict.union acc (run rest (mulVector m x, mulVector m y)))
                (Dict.empty sortFn)
                first

mulEdge : Float4x4 -> Edge -> Edge
mulEdge m (x,y) = (mulVector m x, mulVector m y)

roundPoint : Float4 -> Vec4 Int
roundPoint = scale 1024 >> map round

roundEdge : Edge -> (Vec4 Int, Vec4 Int)
roundEdge (p,q) = (roundPoint p, roundPoint q)

sortFn : Edge -> (Vec4 Int, Vec4 Int)
sortFn (p,q) = min (roundEdge (p,q)) (roundEdge (q,p))

simplicial : Symmetry
simplicial =
    let s = sqrt 5
    in
    [ [ ( ( 1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      , ( (-1, 0, 0, 0)
        , ( 0,-1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      ]
    , [ ( ( 1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      , ( (-1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0,-1, 0)
        , ( 0, 0, 0, 1))
      ]
    , [ ( ( 1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      , ( ( 0, 1, 0, 0)
        , ( 1, 0, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      ]
    , [ ( ( 1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      , ( ( 0, 1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 1, 0, 0, 0)
        , ( 0, 0, 0, 1))
      , ( ( 0, 0, 1, 0)
        , ( 1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0, 0, 1))
      ]
    , [ ( ( 1, 0, 0, 0)
        , ( 0, 1, 0, 0)
        , ( 0, 0, 1, 0)
        , ( 0, 0, 0, 1))
      , ( ( 3/4,-1/4,-1/4,-s/4)
        , (-1/4, 3/4,-1/4,-s/4)
        , (-1/4,-1/4, 3/4,-s/4)
        , (-s/4,-s/4,-s/4,-1/4)
        )
      , ( ( 3/4,-1/4, 1/4, s/4)
        , (-1/4, 3/4, 1/4, s/4)
        , ( 1/4, 1/4, 3/4,-s/4)
        , ( s/4, s/4,-s/4,-1/4)
        )
      , ( ( 3/4, 1/4,-1/4, s/4)
        , ( 1/4, 3/4, 1/4,-s/4)
        , (-1/4, 1/4, 3/4, s/4)
        , ( s/4,-s/4, s/4,-1/4)
        )
      , ( ( 3/4, 1/4, 1/4,-s/4)
        , ( 1/4, 3/4,-1/4, s/4)
        , ( 1/4,-1/4, 3/4, s/4)
        , (-s/4, s/4, s/4,-1/4)
        )
      ]
    ]

tesseractic : Symmetry
tesseractic =
    cubicPrismSym ++
    [ [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((0,1,0,0),(0,0,1,0),(0,0,0,1),(1,0,0,0))
      , ((0,0,1,0),(0,0,0,1),(1,0,0,0),(0,1,0,0))
      , ((0,0,0,1),(1,0,0,0),(0,1,0,0),(0,0,1,0))
      ]
    ]



snub24sym : Symmetry
snub24sym =
    [ [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( (-1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 1, 0, 0, 0 )
        , ( 0,-1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0,-1, 0 )
        , ( 0, 0, 0, 1 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0,-1 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 0, 1, 0, 0 )
        , ( 1, 0, 0, 0 )
        , ( 0, 0, 0, 1 )
        , ( 0, 0, 1, 0 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        , ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 1, 0, 0, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 0, 0, 1, 0 )
        , ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 0, 1 )
        )
      ]
    , [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 1/2, 1/2, 1/2, 1/2 )
        , ( 1/2, 1/2,-1/2,-1/2 )
        , ( 1/2,-1/2, 1/2,-1/2 )
        , ( 1/2,-1/2,-1/2, 1/2 )
        )
      , ( ( 1/2,-1/2,-1/2,-1/2 )
        , (-1/2, 1/2,-1/2,-1/2 )
        , (-1/2,-1/2, 1/2,-1/2 )
        , (-1/2,-1/2,-1/2, 1/2 )
        )
      ]
    ]

regular24sym : Symmetry
regular24sym =
    snub24sym
    ++
    [ [ ( ( 1, 0, 0, 0 )
        , ( 0, 1, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      , ( ( 0, 1, 0, 0 )
        , ( 1, 0, 0, 0 )
        , ( 0, 0, 1, 0 )
        , ( 0, 0, 0, 1 )
        )
      ]
    ]



grandAntiprismSym : Symmetry
grandAntiprismSym =
    let φ = (sqrt 5 + 1) / 2
        m1 = 
            ( (1,         0,         0,      0)
            , (0, 0.4*φ+0.3,-0.3*φ+0.4,(φ-1)/2)
            , (0,-0.3*φ+0.4, 0.1*φ+0.7,    1/2)
            , (0,   (1-φ)/2,      -1/2,    φ/2)
            )
        m2 = 
            ( (    φ/2,       1/2,   (1-φ)/2, 0)
            , (   -1/2, 0.1*φ+0.7, 0.3*φ-0.4, 0)
            , ((φ-1)/2, 0.3*φ-0.4, 0.4*φ+0.3, 0)
            , (      0,         0,         0, 1)
            )
        m3 = Matrix4.mul m1 m2
        m4 = Matrix4.mul m1 (Matrix4.transpose m2) -- == inverse
    in
        [ List.scanl Matrix4.mul Matrix4.identity [m3,m3,m3,m3,m3,m3,m3,m3,m3]
        , List.scanl Matrix4.mul Matrix4.identity [m4,m4,m4,m4,m4,m4,m4,m4,m4]
        ]

regular120sym : Symmetry
regular120sym = snub24sym ++ grandAntiprismSym


duoprismSym : Int -> Int -> Symmetry
duoprismSym m n =
    let s1 = sin (turns (1/toFloat m))
        c1 = cos (turns (1/toFloat m))
        s2 = sin (turns (1/toFloat n))
        c2 = cos (turns (1/toFloat n))
        m1 = ((c1,s1,0,0),(-s1,c1,0,0),(0,0,1,0),(0,0,0,1))
        m2 = ((1,0,0,0),(0,1,0,0),(0,0,c2,s2),(0,0,-s2,c2))
    in    
    [ List.scanl Matrix4.mul Matrix4.identity (List.repeat m m1)
    , List.scanl Matrix4.mul Matrix4.identity (List.repeat n m2)
    ]

antiprismPrismSym : Int -> Symmetry
antiprismPrismSym n =
    let s1 = sin (turns (1/toFloat (2*n)))
        c1 = cos (turns (1/toFloat (2*n)))
        m1 = ((c1,s1,0,0),(-s1,c1,0,0),(0,0,-1,0),(0,0,0,1))
    in 
    [ List.scanl Matrix4.mul Matrix4.identity (List.repeat (2*n) m1)
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,-1))
      ]
    ]
    

tetrahedralPrismSym : Symmetry
tetrahedralPrismSym =
    [ [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,-1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,-1,0,0),(0,0,-1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((-1,0,0,0),(0,-1,0,0),(0,0,1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((0,1,0,0),(1,0,0,0),(0,0,1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((0,1,0,0),(0,0,1,0),(1,0,0,0),(0,0,0,1))
      , ((0,0,1,0),(1,0,0,0),(0,1,0,0),(0,0,0,1))
      ]
    ]

snubCubicPrismSym : Symmetry
snubCubicPrismSym =
    [ [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,-1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,-1,0,0),(0,0,-1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((-1,0,0,0),(0,-1,0,0),(0,0,1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((0,1,0,0),(0,0,1,0),(1,0,0,0),(0,0,0,1))
      , ((0,0,1,0),(1,0,0,0),(0,1,0,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((0,1,0,0),(1,0,0,0),(0,0,-1,0),(0,0,0,1))
      ]
    ]

cubicPrismSym : Symmetry
cubicPrismSym =
    snubCubicPrismSym ++ [[((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1)), ((0,1,0,0),(1,0,0,0),(0,0,1,0),(0,0,0,1))]]


snubDodecahedralPrismSym : Symmetry
snubDodecahedralPrismSym =
    let phi = (sqrt 5 + 1) / 2
        m = 
            ( (1/2,-phi/2,1/(2*phi),0)
            , (phi/2,1/(2*phi),-1/2,0)
            , (1/(2*phi),1/2,phi/2,0)
            , (0,0,0,1)
            )
    in 
    [ [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,-1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((1,0,0,0),(0,-1,0,0),(0,0,-1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((-1,0,0,0),(0,-1,0,0),(0,0,1,0),(0,0,0,1))
      ]
    , [ ((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))
      , ((0,1,0,0),(0,0,1,0),(1,0,0,0),(0,0,0,1))
      , ((0,0,1,0),(1,0,0,0),(0,1,0,0),(0,0,0,1))
      ]
    , List.scanl Matrix4.mul Matrix4.identity [m,m,m,m]
    ]

dodecahedralPrismSym : Symmetry
dodecahedralPrismSym =
    snubDodecahedralPrismSym ++ [[((1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1)), ((-1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1))]]

