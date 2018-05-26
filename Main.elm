import Html exposing (Html)
import Matrix4 exposing (Float4x4)
import GL exposing (Mesh, triangle, toMesh)
import Keyboard
import Window
import Task
import Meshes exposing (getMesh,M,Object)
import Set exposing (Set)
import AnimationFrame
import Time exposing (Time)
import InfiniteList as IL

import Element
import Element.Attributes as Element
import Element.Events as Element
import Element.Input as Input
import Style
import Style.Color
import Style.Border
import Color


type alias Model = 
    { win : Window.Size
    , state : State
    , mat : Float4x4
    , keys : Set Keyboard.KeyCode
    , inputDuoprism1 : String
    , inputDuoprism2 : String
    , inputAntiprismPrism : String
    , fov : Float
    , speed : Float
    }
type State 
    = Select
    | View {object : Object}


type Msg 
    = KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | WindowResize Window.Size
    | SetObject Object
    | Tick Time
    | InputDuoprism1 String
    | InputDuoprism2 String
    | InputAntiprismPrism String
    | SetFov String
    | SetSpeed String

main : Program Never Model Msg
main = Html.program {init = init, update = \msg model -> (update msg model, Cmd.none), view = view, subscriptions = subscriptions}

init : (Model, Cmd Msg)
init = 
    ( { state = Select
      , mat = Matrix4.identity
      , win = Window.Size 0 0 
      , keys = Set.empty
      , inputDuoprism1 = "8"
      , inputDuoprism2 = "8"
      , inputAntiprismPrism = "8"
      , fov = 80
      , speed = 10
      }
    , Task.perform WindowResize Window.size)

relevantKeys : Set Keyboard.KeyCode
relevantKeys = Set.fromList [27,87,83,65,68,69,81,73,74,75,76,79,85]

update : Msg -> Model -> Model
update msg model = 
    case msg of
        WindowResize win -> {model | win = win}
        InputDuoprism1 str -> {model | inputDuoprism1 = str}
        InputDuoprism2 str -> {model | inputDuoprism2 = str}
        InputAntiprismPrism str -> {model | inputAntiprismPrism = str}
        SetFov str -> 
            case String.toFloat str of
                Ok fov -> {model | fov = fov}
                Err _ -> model
        SetSpeed str -> 
            case String.toFloat str of
                Ok speed -> {model | speed = speed}
                Err _ -> model
        SetObject object -> {model | state = View {object = object}}
        KeyDown key -> {model | keys = Set.insert key model.keys}
        KeyUp key -> {model | keys = Set.remove key model.keys}
        Tick dt ->
            let c1 = cos (degrees (model.speed*Time.inSeconds dt))
                s1 = sin (degrees (model.speed*Time.inSeconds dt))
                c2 = cos (degrees (20*Time.inSeconds dt))
                s2 = sin (degrees (20*Time.inSeconds dt))
            in Set.foldl
                (\k m ->
                    let move mat = {m | mat = Matrix4.mul mat m.mat}
                    in case k of
                        27 -> {m | state = Select}
                        87 -> move
                            ( ( 1 , 0 , 0 , 0 )
                            , ( 0 , 1 , 0 , 0 )
                            , ( 0 , 0 , c1, s1)
                            , ( 0 , 0 ,-s1, c1))
                        83 -> move
                            ( ( 1 , 0 , 0 , 0 )
                            , ( 0 , 1 , 0 , 0 )
                            , ( 0 , 0 , c1,-s1)
                            , ( 0 , 0 , s1, c1))
                        65 -> move
                            ( ( c1, 0 , 0 , s1)
                            , ( 0 , 1 , 0 , 0 )
                            , ( 0 , 0 , 1 , 0 )
                            , (-s1, 0 , 0 , c1))
                        68 -> move
                            ( ( c1, 0 , 0 ,-s1)
                            , ( 0 , 1 , 0 , 0 )
                            , ( 0 , 0 , 1 , 0 )
                            , ( s1, 0 , 0 , c1))
                        69 -> move
                            ( ( 1 , 0 , 0 , 0 )
                            , ( 0 , c1, 0 , s1)
                            , ( 0 , 0 , 1 , 0 )
                            , ( 0 ,-s1, 0 , c1))
                        81 -> move
                            ( ( 1 , 0 , 0 , 0 )
                            , ( 0 , c1, 0 ,-s1)
                            , ( 0 , 0 , 1 , 0 )
                            , ( 0 , s1, 0 , c1))

                        74 -> move
                            ( ( c2, 0 ,-s2, 0 )
                            , ( 0 , 1 , 0 , 0 )
                            , ( s2, 0 , c2, 0 )
                            , ( 0 , 0 , 0 , 1 ))
                        76 -> move
                            ( ( c2, 0 , s2, 0 )
                            , ( 0 , 1 , 0 , 0 )
                            , (-s2, 0 , c2, 0 )
                            , ( 0 , 0 , 0 , 1 ))
                        75 -> move
                            ( ( 1 , 0 , 0 , 0 )
                            , ( 0 , c2,-s2, 0 )
                            , ( 0 , s2, c2, 0 )
                            , ( 0 , 0 , 0 , 1 ))
                        73 -> move
                            ( ( 1 , 0 , 0 , 0 )
                            , ( 0 , c2, s2, 0 )
                            , ( 0 ,-s2, c2, 0 )
                            , ( 0 , 0 , 0 , 1 ))
                        85 -> move
                            ( ( c2,-s2, 0 , 0 )
                            , ( s2, c2, 0 , 0 )
                            , ( 0 , 0 , 1 , 0 )
                            , ( 0 , 0 , 0 , 1 ))
                        79 -> move
                            ( ( c2, s2, 0 , 0 )
                            , (-s2, c2, 0 , 0 )
                            , ( 0 , 0 , 1 , 0 )
                            , ( 0 , 0 , 0 , 1 ))
                        _ -> model)
                model
                model.keys



type Style
    = None
    | Input
    | Box

styleSheet : Style.StyleSheet Style v
styleSheet =
    Style.styleSheet
        [ Style.style None []
        , Style.style Input
            [ Style.Color.border Color.gray
            , Style.Border.all 1
            , Style.Border.solid
            ]
        , Style.style Box
            [ Style.Color.background Color.gray
            ]
        ]

description : String
description = 
    """These are all of the uniform polytopes in four dimensions. Click one to view it. 
When viewing, you are in the three-dimensional surface of a four dimensional hypersphere.
The polytope being viewed is "balloned out" onto the hypersphere.

You can use WASDQE to move, and IJKLUO to turn. Use the escape key to return to this screen.

Warning: For reasons I have not yet debugged, the 120 and 600-cell variants take a very long time to generate. 
The worst is the omnitruncated 120-cell, which on my computer takes 18 minutes!
Fortunately, the polytope will not have to regenerate unless you reload the page.
"""

view : Model -> Html Msg
view model =
    case model.state of
        View info ->
            GL.render 
                model.win.width
                model.win.height
                model.fov
                (getMesh info.object)
                model.mat
        Select -> 
            Element.layout styleSheet <|
                Element.el None [Element.center] <|
                Element.column None
                    [ Element.spacing 10, Element.center ]
                    [ Element.textLayout Box 
                        [ Element.padding 10
                        ]
                        [ Element.text description
                        ]
                    , Element.row None [ Element.spacing 10 ]
                        [ Element.column None []
                            [ Element.text "1-3 seconds to generate"
                            , button (SetObject         .regular5cell)         "regular 5-cell"
                            , button (SetObject       .rectified5cell)       "rectified 5-cell"
                            , button (SetObject       .truncated5cell)       "truncated 5-cell"
                            , button (SetObject     .cantellated5cell)     "cantellated 5-cell"
                            , button (SetObject      .runcinated5cell)      "runcinated 5-cell"
                            , button (SetObject     .bitruncated5cell)     "bitruncated 5-cell"
                            , button (SetObject  .cantitruncated5cell)  "cantitruncated 5-cell"
                            , button (SetObject  .runcitruncated5cell)  "runcitruncated 5-cell"
                            , button (SetObject   .omnitruncated5cell)   "omnitruncated 5-cell"
                            ]
                        , Element.column None []
                            [ Element.text "2-6 seconds to generate"
                            , button (SetObject         .regular8cell)         "regular 8-cell"
                            , button (SetObject       .rectified8cell)       "rectified 8-cell"
                            , button (SetObject        .regular16cell)        "regular 16-cell"
                            , button (SetObject       .truncated8cell)       "truncated 8-cell"
                            , button (SetObject     .cantellated8cell)     "cantellated 8-cell"
                            , button (SetObject      .runcinated8cell)      "runcinated 8-cell"
                            , button (SetObject     .bitruncated8cell)     "bitruncated 8-cell"
                            , button (SetObject      .truncated16cell)      "truncated 16-cell"
                            , button (SetObject  .cantitruncated8cell)  "cantitruncated 8-cell"
                            , button (SetObject  .runcitruncated8cell)  "runcitruncated 8-cell"
                            , button (SetObject .runcitruncated16cell) "runcitruncated 16-cell"
                            , button (SetObject   .omnitruncated8cell)   "omnitruncated 8-cell"
                            ]
                        , Element.column None []
                            [ Element.text "3-15 seconds to generate"
                            , button (SetObject        .regular24cell)        "regular 24-cell"
                            , button (SetObject      .rectified24cell)      "rectified 24-cell"
                            , button (SetObject      .truncated24cell)      "truncated 24-cell"
                            , button (SetObject    .cantellated24cell)    "cantellated 24-cell"
                            , button (SetObject     .runcinated24cell)     "runcinated 24-cell"
                            , button (SetObject    .bitruncated24cell)    "bitruncated 24-cell"
                            , button (SetObject .cantitruncated24cell) "cantitruncated 24-cell"
                            , button (SetObject .runcitruncated24cell) "runcitruncated 24-cell"
                            , button (SetObject  .omnitruncated24cell)  "omnitruncated 24-cell"
                            , button (SetObject           .snub24cell)           "snub 24-cell"
                            ]
                        , Element.column None []
                            [ Element.text "18-1000+ seconds to generate :("
                            , button (SetObject        .regular120cell)        "regular 120-cell"
                            , button (SetObject      .rectified120cell)      "rectified 120-cell"
                            , button (SetObject      .rectified600cell)      "rectified 600-cell"
                            , button (SetObject        .regular600cell)        "regular 600-cell"
                            , button (SetObject      .truncated120cell)      "truncated 120-cell"
                            , button (SetObject    .cantellated120cell)    "cantellated 120-cell"
                            , button (SetObject     .runcinated120cell)     "runcinated 120-cell"
                            , button (SetObject    .bitruncated120cell)    "bitruncated 120-cell"
                            , button (SetObject    .cantellated600cell)    "cantellated 600-cell"
                            , button (SetObject      .truncated600cell)      "truncated 600-cell"
                            , button (SetObject .cantitruncated120cell) "cantitruncated 120-cell"
                            , button (SetObject .runcitruncated120cell) "runcitruncated 120-cell"
                            , button (SetObject .runcitruncated600cell) "runcitruncated 600-cell"
                            , button (SetObject .cantitruncated600cell) "cantitruncated 600-cell"
                            , button (SetObject  .omnitruncated120cell)  "omnitruncated 120-cell"
                            ]
                        ]
                    , Element.spacer 1
                    , Element.text "These take a couple of seconds to generate."
                    , Element.column None []
                        [ button (SetObject                  .grandAntiprism)                   "grand antiprism"
                        ]
                    , Element.row None [ Element.spacing 10 ]
                        [ Element.column None []
                            [ button (SetObject                .tetrahedralPrism)                 "tetrahedral prism"
                            , button (SetObject       .truncatedTetrahedralPrism)       "truncated tetrahedral prism"
                            ]
                        , Element.column None []
                            [ button (SetObject                    .regular8cell)                     "cubical prism"
                            , button (SetObject             .cubeoctahedralPrism)              "cubeoctahedral prism"
                            , button (SetObject                 .octahedralPrism)                  "octahedral prism"
                            , button (SetObject       .rhombicubeoctahedralPrism)        "rhombicubeoctahedral prism"
                            , button (SetObject             .truncatedCubicPrism)             "truncated cubic prism"
                            , button (SetObject        .truncatedOctahedralPrism)        "truncated octahedral prism"
                            , button (SetObject    .truncatedCubeoctahedralPrism)    "truncated cubeoctahedral prism"
                            , button (SetObject                  .snubCubicPrism)                  "snub cubic prism"
                            ]
                        , Element.column None []
                            [ button (SetObject               .dodecahedralPrism)                "dodecahedral prism"
                            , button (SetObject          .icosidodecahedralPrism)           "icosidodecahedral prism"
                            , button (SetObject                .icosahedralPrism)                 "icosahedral prism"
                            , button (SetObject      .truncatedDodecahedralPrism)      "truncated dodecahedral prism"
                            , button (SetObject     .rhombicosidodecahedralPrism)      "rhombicosidodecahedral prism"
                            , button (SetObject       .truncatedIcosahedralPrism)       "truncated icosahedral prism"
                            , button (SetObject .truncatedIcosidodecahedralPrism) "truncated icosidodecahedral prism"
                            , button (SetObject           .snubDodecahedralPrism)           "snub dodecahedral prism"
                            ]
                        ]
                    , Element.row None
                        [ Element.spacing 10 ]
                        [ case String.toInt model.inputAntiprismPrism of
                            Ok n ->
                                if n >= 3
                                then button (SetObject (.antiprismPrism >> IL.get (n-3))) "antiprism prism"
                                else Element.empty
                            Err _ -> Element.empty
                        , input InputAntiprismPrism model.inputAntiprismPrism
                        ]
                    , Element.row None
                        [ Element.spacing 10 ]
                        [ case (String.toInt model.inputDuoprism1, String.toInt model.inputDuoprism2) of
                            (Ok m,Ok n) -> 
                                if m >= 3 && n >= 3
                                then button (SetObject (.duoprism >> IL.get (m-3) >> IL.get (n-3))) "duoprism"
                                else Element.empty
                            _ -> Element.empty
                        , input InputDuoprism1 model.inputDuoprism1
                        , input InputDuoprism2 model.inputDuoprism2
                        ]
                    , Element.spacer 2
                    , Element.row None
                        [ Element.spacing 10 ]
                        [ Element.text "FOV"
                        , input SetFov (toString model.fov)
                        ]
                    , Element.row None
                        [ Element.spacing 10 ]
                        [ Element.text "Speed"
                        , input SetSpeed (toString model.speed)
                        ]
                    
                    ]

button : msg -> String -> Element.Element Style v msg
button msg str =
    Element.button None
        [Element.onClick msg]
        (Element.text str)

input : (String -> msg) -> String -> Element.Element Style v msg
input msg str =
    Element.el None []
        ( Input.text Input
            [ Element.width (Element.px 100) ]
            { onChange = msg
            , value = str 
            , label = Input.hiddenLabel "" 
            , options = [] 
            }
        )


subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        , Window.resizes WindowResize
        , case model.state of
            Select -> Sub.none
            View _ -> 
                if Set.isEmpty (Set.intersect relevantKeys model.keys)
                then Sub.none
                else AnimationFrame.diffs Tick
        ]











