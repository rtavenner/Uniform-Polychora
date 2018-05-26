--5-cell
--5-cell rectified
--5-cell truncated
--5-cell cantellated
--5-cell runcinated
--5-cell bitruncated
--5-cell cantitruncated
--5-cell runcitruncated
--5-cell omnitruncated
--8-cell
--8-cell rectified
--16-cell
--8-cell truncated
--8-cell cantellated
--8-cell runcinated
--8-cell bitruncated
--16-cell truncated
--8-cell cantitruncated
--8-cell runcitruncated
--16-cell runcitruncated
--8-cell omnitruncated
--24-cell
--24-cell rectified
--24-cell truncated
--24-cell cantellated
--24-cell runcinated
--24-cell bitruncated
--24-cell cantitruncated
--24-cell runcitruncated
--24-cell omnitruncated
--24-cell snub
--120-cell
--120-cell rectified
--600-cell rectified
--600-cell
--120-cell truncated
--120-cell cantellated
--120-cell runcinated
--120-cell bitruncated
--600-cell cantellated
--600-cell truncated
--120-cell cantitruncated
--120-cell runcitruncated
--600-cell runcitruncated
--600-cell cantitruncated
--120-cell omnitruncated
--antiprism grand
--prism tetrahedral
--prism tetrahedral truncated
--prism cubeoctahedral
--prism octahedral
--prism rhombicubeoctahedral
--prism cubic truncated
--prism octahedral truncated
--prism cubeoctahedral truncated
--prism cubic snub
--prism dodecahedral
--prism icosidodecahedral
--prism icosahedral
--prism dodecahedral truncated
--prism rhombicosidodecahedral
--prism icosahedral truncated
--prism icosidodecahedral truncated
--prism dodecahedral snub

module Meshes exposing (getMesh, PolytopeData, M, Object)

import Symmetry exposing (..)
import GL exposing (toMesh,triangle)
import Array.Hamt as Array exposing (Array)
import ArrayExtra as Array
import Lazy exposing (..)
import Vector4 exposing (..)
import Math exposing (..)
import InfiniteList as IL exposing (InfiniteList)

type alias Object = PolytopeData -> M

type alias M = Lazy GL.Mesh
type alias PolytopeData =
    {         regular5cell: M
    ,       rectified5cell: M
    ,       truncated5cell: M
    ,     cantellated5cell: M
    ,      runcinated5cell: M
    ,     bitruncated5cell: M
    ,  cantitruncated5cell: M
    ,  runcitruncated5cell: M
    ,   omnitruncated5cell: M


    ,         regular8cell: M
    ,       rectified8cell: M
    ,        regular16cell: M
    ,       truncated8cell: M
    ,     cantellated8cell: M
    ,      runcinated8cell: M
    ,     bitruncated8cell: M
    ,      truncated16cell: M
    ,  cantitruncated8cell: M
    ,  runcitruncated8cell: M
    , runcitruncated16cell: M
    ,   omnitruncated8cell: M


    ,        regular24cell: M
    ,      rectified24cell: M
    ,      truncated24cell: M
    ,    cantellated24cell: M
    ,     runcinated24cell: M
    ,    bitruncated24cell: M
    , cantitruncated24cell: M
    , runcitruncated24cell: M
    ,  omnitruncated24cell: M
    ,           snub24cell: M


    ,       regular120cell: M
    ,     rectified120cell: M
    ,     rectified600cell: M
    ,       regular600cell: M
    ,     truncated120cell: M
    ,   cantellated120cell: M
    ,    runcinated120cell: M
    ,   bitruncated120cell: M
    ,   cantellated600cell: M
    ,     truncated600cell: M
    ,cantitruncated120cell: M
    ,runcitruncated120cell: M
    ,runcitruncated600cell: M
    ,cantitruncated600cell: M
    , omnitruncated120cell: M

    ,       grandAntiprism: M

    , tetrahedralPrism : M
    , truncatedTetrahedralPrism : M
    , cubeoctahedralPrism : M
    , octahedralPrism : M
    , rhombicubeoctahedralPrism : M
    , truncatedCubicPrism : M
    , truncatedOctahedralPrism : M
    , truncatedCubeoctahedralPrism : M
    , snubCubicPrism : M
    , dodecahedralPrism : M
    , icosidodecahedralPrism : M
    , icosahedralPrism : M
    , truncatedDodecahedralPrism : M
    , rhombicosidodecahedralPrism : M
    , truncatedIcosahedralPrism : M
    , truncatedIcosidodecahedralPrism : M
    , snubDodecahedralPrism : M

    , antiprismPrism : InfiniteList M
    , duoprism : InfiniteList (InfiniteList M)
    }

getMesh : (PolytopeData -> M) -> GL.Mesh
getMesh getter = force (getter meshes)

-- Thanks to http://www.eusebeia.dyndns.org/4d/
-- Also, thanks to Wikipedia.
meshes : PolytopeData
meshes =
    let s2 = sqrt 2
        s5 = sqrt 5
        phi = (s5 + 1) / 2
    in
    { regular5cell = makeMesh 60 simplicial [((0,0,0,-1),(s5/4,s5/4,s5/4,1/4))]
    , rectified5cell = makeMesh 60 simplicial [((0,0,s5,1),(0,s5,0,1))]
    , truncated5cell = makeMesh 60 simplicial 
        [ ((s5,s5,3*s5,3),(-s5,-s5,3*s5,3))
        , ((s5,s5,3*s5,3),(3*s5,s5,s5,3))
        ]
    , cantellated5cell = makeMesh 60 simplicial
        [ ((s5,0,s5,2),(s5,s5,0,2))
        , ((s5,0,s5,2),(s5,-s5,0,2))
        ]
    , runcinated5cell = makeMesh 60 simplicial
        [ ((1,1,0,0),(1,0,1,0))
        , ((1,1,0,0),(1,0,-1,0))
        ]
    , bitruncated5cell = makeMesh 60 simplicial
        [ ((1,-3,1,s5),(-1,-3,-1,s5))
        , ((1,-3,1,s5),(3,-1,1,s5))
        ]
    , cantitruncated5cell = makeMesh 60 simplicial
        [ ((s5,0,2*s5,3),(0,s5,2*s5,3))
        , ((s5,0,2*s5,3),(2*s5,0,s5,3))
        , ((s5,0,2*s5,3),(0,-s5,2*s5,3))
        ]
    , runcitruncated5cell = makeMesh 60 simplicial
        [ ((0,s5,2*s5,1),(s5,0,2*s5,1))
        , ((0,s5,2*s5,1),(0,2*s5,s5,1))
        , ((0,s5,2*s5,1),(-s5,0,2*s5,1))
        ]
    , omnitruncated5cell = makeMesh 60 simplicial
        [ ((1,0,2,s5),(0,1,2,s5))
        , ((1,0,2,s5),(2,0,1,s5))
        , ((1,0,2,s5),(0,-1,2,s5))
        , ((1,0,2,s5),(3/2,-1/2,5/2,s5/2))
        ]

    , regular8cell = makeMesh 45 tesseractic [((1,1,1,1),(-1,1,1,1))]
    , rectified8cell = makeMesh 45 tesseractic [((0,1,1,1),(1,0,1,1))]
    , regular16cell = makeMesh 45 tesseractic [((1,0,0,0),(0,1,0,0))]
    , truncated8cell = makeMesh 45 tesseractic [((1,1+s2,1+s2,1+s2),(1+s2,1,1+s2,1+s2)),((1,1+s2,1+s2,1+s2),(-1,1+s2,1+s2,1+s2))]
    , cantellated8cell = makeMesh 45 tesseractic
        [ ((1,1,1+s2,1+s2),(-1,1,1+s2,1+s2))
        , ((1,1,1+s2,1+s2),(1,1+s2,1,1+s2))
        ]
    , runcinated8cell = makeMesh 45 tesseractic
        [ ((1,1,1,1+s2),(-1,1,1,1+s2))
        , ((1,1,1,1+s2),(1,1,1+s2,1))
        ]
    , bitruncated8cell = makeMesh 45 tesseractic
        [ ((0,1,2,2),(1,0,2,2))
        , ((0,1,2,2),(0,2,1,2))
        ]
    , truncated16cell = makeMesh 45 tesseractic [((0,0,1,2),(0,0,2,1)),((0,0,1,2),(0,1,0,2))]
    , cantitruncated8cell = makeMesh 45 tesseractic
        [ ((1,1+s2,1+2*s2,1+2*s2),(-1,1+s2,1+2*s2,1+2*s2))
        , ((1,1+s2,1+2*s2,1+2*s2),(1,1+2*s2,1+s2,1+2*s2))
        , ((1,1+s2,1+2*s2,1+2*s2),(1+s2,1,1+2*s2,1+2*s2))
        ]
    , runcitruncated8cell = makeMesh 45 tesseractic
        [ ((1,1+s2,1+s2,1+2*s2),(-1,1+s2,1+s2,1+2*s2))
        , ((1,1+s2,1+s2,1+2*s2),(1+s2,1,1+s2,1+2*s2))
        , ((1,1+s2,1+s2,1+2*s2),(1,1+s2,1+2*s2,1+s2))
        ]
    , runcitruncated16cell = makeMesh 45 tesseractic
        [ ((1,1,1+s2,1+2*s2),(-1,1,1+s2,1+2*s2))
        , ((1,1,1+s2,1+2*s2),(1,1+s2,1,1+2*s2))
        , ((1,1,1+s2,1+2*s2),(1,1,1+2*s2,1+s2))
        ]
    , omnitruncated8cell = makeMesh 45 tesseractic
        [ ((1,1+s2,1+2*s2,1+3*s2),(-1,1+s2,1+2*s2,1+3*s2))
        , ((1,1+s2,1+2*s2,1+3*s2),(1+s2,1,1+2*s2,1+3*s2))
        , ((1,1+s2,1+2*s2,1+3*s2),(1,1+2*s2,1+s2,1+3*s2))
        , ((1,1+s2,1+2*s2,1+3*s2),(1,1+s2,1+3*s2,1+2*s2))
        ]



    , regular24cell = makeMesh 30 regular24sym [((0,0,0,2),(1,1,1,1))]
    , rectified24cell = makeMesh 30 regular24sym [((1,1,1,3),(-1,1,1,3))]
    , truncated24cell = makeMesh 30 regular24sym 
        [ ((2,2,2,4),(1,1,1,5))
        , ((1,1,1,5),(-1,1,1,5))
        ]
    , cantellated24cell = makeMesh 30 regular24sym
        [ ((0,s2,s2,2+2*s2), (s2,0,s2,2+2*s2))
        , ((0,s2,s2,2+2*s2), (1,1+s2,1+s2,1+2*s2))
        ]
    , runcinated24cell = makeMesh 30 regular24sym
        [ ((1,1,1+s2,1+s2),(-1,1,1+s2,1+s2))
        , ((1,1,1+s2,1+s2),(1,1+s2,1,1+s2))
        ]
    , bitruncated24cell = makeMesh 30 regular24sym
        [ ((0,2+s2,2+s2,2+2*s2),(0,2+s2,2+2*s2,2+s2))
        , ((0,2+s2,2+s2,2+2*s2),(1,1+s2,1+s2,3+2*s2))
        ]
    , cantitruncated24cell = makeMesh 30 regular24sym
        [ ((0,2+s2,2+2*s2,2+3*s2),(0,2+2*s2,2+s2,2+3*s2))
        , ((0,2+s2,2+2*s2,2+3*s2),(0,2+s2,2+3*s2,2+2*s2))
        , ((0,2+s2,2+2*s2,2+3*s2),(1,1+s2,1+2*s2,3+3*s2))
        ]
    , runcitruncated24cell = makeMesh 30 regular24sym
        [ ((0,s2,2*s2,2+3*s2), (0,2*s2,s2,2+3*s2))
        , ((0,s2,2*s2,2+3*s2), (s2,0,2*s2,2+3*s2))
        , ((0,s2,2*s2,2+3*s2),(1,1+s2,1+2*s2,1+3*s2))
        ]
    , omnitruncated24cell = makeMesh 30 regular24sym
        [ ((1,3+s2,3+2*s2,3+3*s2),(1,3+2*s2,3+s2,3+3*s2))
        , ((1,3+s2,3+2*s2,3+3*s2),(1,3+s2,3+3*s2,3+2*s2))
        , ((1,3+s2,3+2*s2,3+3*s2),(2,2+s2,2+2*s2,4+3*s2))
        , ((2,2+s2,2+2*s2,4+3*s2),(1,1+s2,1+2*s2,5+3*s2))
        ]
    , snub24cell = makeMesh 30 snub24sym
        [ ((0,(phi-1),1,phi),(0,-(phi-1),1,phi))
        , ((0,(phi-1),1,phi),((phi-1),1,0,phi))
        ]


    , regular120cell = makeMesh 15 regular120sym
        [ ((2-phi,1,phi+1,0),(phi-2,1,phi+1,0))
        ]
    , rectified120cell = makeMesh 15 regular120sym
        [ ((0,2*(phi-1),2*phi,0),((phi-1),1,2*phi,(2-phi)))
        ]
    , rectified600cell = makeMesh 15 regular120sym
        [ ((0,phi-1,1,phi+2), (0,1-phi,1,phi+2))
        ]
    , regular600cell = makeMesh 15 regular120sym
        [ ((0,(phi-1),1,phi),(0,-(phi-1),1,phi))
        ]
    , truncated120cell = makeMesh 15 regular120sym
        [ ((2-phi,2*phi-1,(3*phi+1),0), (phi-2,2*phi-1,(3*phi+1),0))
        , ((2-phi,2*phi-1,(3*phi+1),0), (1,2,3*phi+1,2-phi))
        ]
    , cantellated120cell = makeMesh 15 regular120sym
        [ ((2-phi, 2-phi, phi, 3*phi), (phi-2, 2-phi, phi, 3*phi))
        , ((2-phi, 2-phi, phi, 3*phi), (2-phi, phi-2, phi, 3*phi))
        ]
    , runcinated120cell = makeMesh 15 regular120sym
        [ ((2-phi,2-phi,phi,2+phi), (phi-2,2-phi,phi,2+phi))
        , ((2-phi,2-phi,phi,2+phi), (2-phi,phi-2,phi,2+phi))
        ]
    , bitruncated120cell = makeMesh 15 regular120sym
        [ ((0,1,4+5*phi,1+5*phi),(0,-1,4+5*phi,1+5*phi))
        , ((0,1,3*phi,3+7*phi),(0,-1,3*phi,3+7*phi))
        ]
    , cantellated600cell = makeMesh 15 regular120sym
        [ ((1,1,1+4*phi,3+4*phi), (-1,1,1+4*phi,3+4*phi))
        , ((1,1,1+4*phi,3+4*phi), (1,-1,1+4*phi,3+4*phi))
        ]
    , truncated600cell = makeMesh 15 regular120sym
        [ ((0,phi-1,3,3*phi), (0,1-phi,3,3*phi))
        , ((0,phi-1,1,phi+4), (0,1-phi,1,phi+4))
        ]
    , cantitruncated120cell = makeMesh 15 regular120sym
        [ ((1,1,1+4*phi,5+10*phi),(-1,1,1+4*phi,5+10*phi))
        , ((1,1,1+4*phi,5+10*phi),(1,-1,1+4*phi,5+10*phi))
        , ((1,1,1+4*phi,5+10*phi),(phi^2,2,phi^4,5+10*phi))
        ]
    , runcitruncated120cell = makeMesh 15 regular120sym
        [ ((1,1,3*phi^3,5+6*phi), (-1,1,3*phi^3,5+6*phi))
        , ((1,1,1+4*phi,phi^6), (-1,1,1+4*phi,phi^6))
        , ((1,1,1+4*phi,phi^6), (1,-1,1+4*phi,phi^6))
        ]
    , runcitruncated600cell = makeMesh 15 regular120sym
        [ ((2-phi,2-phi,phi,5*phi-2), (2-phi,phi-2,phi,5*phi-2)) 
        , ((2-phi,2-phi,3*phi-2,phi+4), (phi-2,2-phi,3*phi-2,phi+4))
        , ((2-phi,2-phi,3*phi-2,phi+4), (2-phi,phi-2,3*phi-2,phi+4)) 
        ]
    , cantitruncated600cell = makeMesh 15 regular120sym
        [ ((1,1,1+6*phi,5+6*phi),(-1,1,1+6*phi,5+6*phi))
        , ((1,1,1+6*phi,5+6*phi),(1,-1,1+6*phi,5+6*phi))
        , ((1,1,1+6*phi,5+6*phi),(2,2,2+6*phi,2*phi^4))
        ]
    , omnitruncated120cell = makeMesh 15 regular120sym
        [ ((1,1,3+8*phi,7+8*phi), (-1,1,3+8*phi,7+8*phi))
        , ((1,1,3+8*phi,7+8*phi), (1,-1,3+8*phi,7+8*phi))
        , ((1,1,1+4*phi,5+12*phi), (-1,1,1+4*phi,5+12*phi))
        , ((1,1,1+4*phi,5+12*phi), (1,-1,1+4*phi,5+12*phi))
        ]


    , grandAntiprism = makeMesh 30 grandAntiprismSym
        [ (((phi-1),-1,0,phi),(-(phi-1),-1,0,phi))
        , ((phi,0,-1,(phi-1)),(phi,0,-1,-(phi-1)))

        , ((0,-(phi-1),1,phi),(0,-1,phi,(phi-1)))
        , ((phi,-1,-(phi-1),0),((phi-1),-phi,-1,0))

        , ((0,(phi-1),-1,phi),(0,1,-phi,(phi-1)))
        , ((phi,1,(phi-1),0),((phi-1),phi,1,0))
        
        , ((0,(phi-1),-1,phi),((phi-1),1,0,phi))
        , ((phi,1,(phi-1),0),(phi,0,1,(phi-1)))
        
        , ((0,-(phi-1),1,-phi),((phi-1),-1,0,-phi))
        , ((-phi,-1,-(phi-1),0),(-phi,0,-1,(phi-1)))
        ]

    , tetrahedralPrism = makeMesh 60 tetrahedralPrismSym
        [ ((1,1,1,s2),(1,1,1,-s2))
        , ((1,1,1,s2),(1,-1,-1,s2))
        ]
    , truncatedTetrahedralPrism = makeMesh 60 tetrahedralPrismSym
        [ ((3,1,1,s2),(3,1,1,-s2))
        , ((3,1,1,s2),(1,3,1,s2))
        , ((3,1,1,s2),(3,-1,-1,s2))
        ]
    , cubeoctahedralPrism = makeMesh 60 cubicPrismSym
        [ ((1,1,0,s2),(1,1,0,-s2))
        , ((1,1,0,s2),(1,0,1,s2))
        ]
    , octahedralPrism = makeMesh 60 cubicPrismSym
        [ ((1,0,0,s2),(1,0,0,-s2))
        , ((1,0,0,s2),(0,1,0,s2))
        ]
    , rhombicubeoctahedralPrism = makeMesh 60 cubicPrismSym
        [ ((1,1,1+s2,1),(1,1,1+s2,-1))
        , ((1,1,1+s2,1),(1,-1,1+s2,1))
        , ((1,1,1+s2,1),(1,1+s2,1,1))
        ]
    , truncatedCubicPrism = makeMesh 60 cubicPrismSym
        [ ((1,1+s2,1+s2,1),(1,1+s2,1+s2,-1))
        , ((1,1+s2,1+s2,1),(-1,1+s2,1+s2,1))
        , ((1,1+s2,1+s2,1),(1+s2,1,1+s2,1))
        ]
    , truncatedOctahedralPrism = makeMesh 60 cubicPrismSym
        [ ((0,s2,2*s2,1),(0,s2,2*s2,-1))
        , ((0,s2,2*s2,1),(s2,0,2*s2,1))
        , ((0,s2,2*s2,1),(0,2*s2,s2,1))
        ]
    , truncatedCubeoctahedralPrism = makeMesh 60 cubicPrismSym
        [ ((1,1+s2,1+2*s2,1),(1,1+s2,1+2*s2,-1))
        , ((1,1+s2,1+2*s2,1),(-1,1+s2,1+2*s2,1))
        , ((1,1+s2,1+2*s2,1),(1+s2,1,1+2*s2,1))
        , ((1,1+s2,1+2*s2,1),(1,1+2*s2,1+s2,1))
        ]
    , snubCubicPrism = 
        let ξ = 0.54368901 -- ξ^3 + ξ^2 + ξ = 1
            h = distance (1/ξ,1,ξ,0) (1/ξ,ξ,-1,0) / 2
        in makeMesh 60 snubCubicPrismSym
            [ ((1/ξ,1,ξ,h),(1/ξ,1,ξ,-h))
            , ((1/ξ,1,ξ,h),(1/ξ,ξ,-1,h))
            , ((1/ξ,1,ξ,h),(1,1/ξ,-ξ,h))
            , ((1/ξ,1,ξ,h),(ξ,1/ξ,1,h))
            ]
    , dodecahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((0,1,phi^2,1),(0,1,phi^2,-1))
        , ((0,1,phi^2,1),(0,-1,phi^2,1))
        ]
    , icosidodecahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((0,0,1,1/(2*phi)),(0,0,1,-1/(2*phi)))
        , ((0,0,1,1/(2*phi)),(1/(2*phi),1/2,phi/2,1/(2*phi)))
        ]
    , icosahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((1,0,phi,1),(1,0,phi,-1))
        , ((1,0,phi,1),(-1,0,phi,1))
        ]
    , truncatedDodecahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((0,1,1+3*phi,1),(0,1,1+3*phi,-1))
        , ((0,1,1+3*phi,1),(0,-1,1+3*phi,1))
        , ((0,1,1+3*phi,1),(1,1+phi,2+2*phi,1))
        ]
    , rhombicosidodecahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((1,1,phi^3,1),(1,1,phi^3,-1))
        , ((1,1,phi^3,1),(1,-1,phi^3,1))
        , ((1,1,phi^3,1),(-1,1,phi^3,1))
        ]
    , truncatedIcosahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((1,0,3*phi,1),(1,0,3*phi,-1))
        , ((1,0,3*phi,1),(-1,0,3*phi,1))
        , ((1,0,3*phi,1),(2,phi,phi^3,1))
        ]
    , truncatedIcosidodecahedralPrism = makeMesh 30 dodecahedralPrismSym
        [ ((1,1,4*phi+1,1),(1,1,4*phi+1,-1))
        , ((1,1,4*phi+1,1),(1,-1,4*phi+1,1))
        , ((1,1,4*phi+1,1),(-1,1,4*phi+1,1))
        , ((1,1,4*phi+1,1),(2,phi^2,phi^4,1))
        ]
    , snubDodecahedralPrism =
        let ξ = 1.715561499697368 -- ξ^3 - 2ξ = phi
            a = ξ - 1/ξ
            b = ξ*phi + phi^2 + phi/ξ
            h = distance (2*a,2,2*b,0) (a + b/phi + phi, -a*phi + b + 1/phi, a/phi + b*phi - 1,0) / 2
        in makeMesh 30 snubDodecahedralPrismSym
            [ ((2*a,2,2*b,h), (2*a,2,2*b,-h))
            , ((2*a,2,2*b,h), (a + b/phi + phi, -a*phi + b + 1/phi, a/phi + b*phi - 1,h))
            , ((2*a,2,2*b,h), (-2*a,-2,2*b,h))
            , ((-2*a,-2,2*b,h), (a + b/phi - phi, a*phi - b + 1/phi, a/phi + b*phi + 1,h))
            ]



    , antiprismPrism =
        IL.countFrom 3
        |> IL.map (\n ->
            let s4 = sin (turns (1/toFloat (4*n)))
                c4 = cos (turns (1/toFloat (4*n)))
                s2 = sin (turns (1/toFloat (2*n)))
                c2 = cos (turns (1/toFloat (2*n)))
                s1 = sin (turns (1/toFloat (1*n)))
                c1 = cos (turns (1/toFloat (1*n)))
                r2 = 1 / s2^2
                h2 = 1 - 1 / (4*c4^2)
                a = sqrt (r2/(1+r2+h2))
                b = sqrt (h2/(1+r2+h2))
                c = sqrt ( 1/(1+r2+h2))
            in makeMesh 60
                (antiprismPrismSym n)
                [ ((a,0,b,c),(a,0,b,-c))
                , ((a,0,b,c),(c2*a,s2*a,-b,c))
                , ((a,0,b,c),(c1*a,s1*a,b,c))
                ])
    , duoprism = 
        IL.countFrom 3
        |> IL.map (\m -> 
            IL.countFrom 3
            |> IL.map (\n ->
                let s1 = sin (turns (1/toFloat m))
                    c1 = cos (turns (1/toFloat m))
                    s2 = sin (turns (1/toFloat n))
                    c2 = cos (turns (1/toFloat n))
                    r1sqrd = 1 / (sin (turns (1/(2*toFloat m))) ^ 2)
                    r2sqrd = 1 / (sin (turns (1/(2*toFloat n))) ^ 2)
                    a = sqrt (r1sqrd / (r1sqrd + r2sqrd))
                    b = sqrt (r2sqrd / (r1sqrd + r2sqrd))
                in makeMesh 60
                    (duoprismSym m n)
                    [ ((a,0,b,0), (a*c1,a*s1,b,0))
                    , ((a,0,b,0), (a,0,b*c2,b*s2))
                    ]))
    }











makeMesh : Int -> Symmetry -> List Edge -> Lazy GL.Mesh
makeMesh rodQuality sym edges =
    lazy <| \_ -> 
        Array.concatMap (runSymmetry sym) (Array.map (\(x,y) -> (normalize x, normalize y)) (Array.fromList edges))
        |> Array.concatMap (uncurry (rod rodQuality))
        |> toMesh

rod : Int -> Float4 -> Float4 -> Array (GL.Vertex,GL.Vertex,GL.Vertex)
rod rodQuality =
    let n = rodQuality
        width = 1/60
        table = 
            Array.map 
                ( toFloat 
                    >> (\x -> turns (x/toFloat n)) 
                    >> (\x -> (sin x * width, cos x * width))) 
                (Array.fromList (List.range 1 n))
        rotate a = 
            Array.append 
                (case Array.get (Array.length a - 1) a of
                    Just x -> Array.repeat 1 x 
                    Nothing -> Array.empty) 
                (Array.slice 0 -1 a)
        f p q =
            if distance p q > 0.5
            then let r = normalize (add p q)
                in Array.append (f p r) (f r q)
            else let 
                (u,v) = perps p q
                vs = Array.map (\(x,y) -> add (scale x u) (scale y v)) table
                ps = Array.map (add p) vs
                qs = Array.map (add q) vs
            in Array.concat
                (Array.map4 
                    (\a b c d -> Array.fromList [triangle a b c, triangle c d a])
                    ps
                    qs
                    (rotate qs)
                    (rotate ps))
    in f
