module GL exposing (Mesh,render,Vertex,Uniforms,triangle,toMesh)

import WebGL
import Math.Matrix4
import Math.Vector3
import Math.Vector4
import Html
import Html.Attributes exposing (width, height, style)
import Math exposing (crossNormalized)
import Array.Hamt as Array exposing (Array)

import Convert exposing (convert)

type alias Mesh = List (WebGL.Mesh Vertex)

render : 
    Int 
    -> Int 
    -> Float
    -> Mesh 
    ->  ((Float,Float,Float,Float)
        ,(Float,Float,Float,Float)
        ,(Float,Float,Float,Float)
        ,(Float,Float,Float,Float)) 
    -> Html.Html msg
render x y fov mesh rotation =
    let uniforms = 
            { perspective = Math.Matrix4.makePerspective fov (toFloat x / toFloat y) 0.01 100
            , rotation = convert rotation 
            }
    in WebGL.toHtml
        [ width  (x-2)
        , height (y-2)
        , style [ ( "display", "block" ), ("border", "1px solid black"), ("background-color", "black") ]
        ]
        (List.map
            (\m -> WebGL.entity
                vertexShader
                fragmentShader
                m
                uniforms)
            mesh)
        --[ WebGL.entity
        --    vertexShader
        --    fragmentShader
        --    mesh
        --    { perspective = Math.Matrix4.makePerspective 80 (toFloat x / toFloat y) 0.01 100, rotation = convert rotation }
        --]

vertexShader : WebGL.Shader Vertex Uniforms Varying
vertexShader = 
    [glsl|
        attribute vec4 pos;
        attribute vec4 normal;
        uniform mat4 perspective;
        uniform mat4 rotation;
        varying vec3 vcolor;
        void main () {
            vec4 pos2 = rotation * pos;
            vec4 normal2 = rotation * normal;
            gl_Position = perspective * (pos2 + vec4(0,0,0,1));
            float cosAngle = dot(normal2,normalize(vec4(-pos2.w*pos2.xyz,dot(pos2.xyz,pos2.xyz))));
            vcolor = abs(cosAngle) * (1.0 + pos2.w)*0.5 * vec3(1,1,1);
        }
    |]

fragmentShader : WebGL.Shader {} Uniforms Varying
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = clamp(vec4(vcolor, 1), 0.0, 0.9);
        }
    |]

type alias Vertex = { pos : Math.Vector4.Vec4, normal : Math.Vector4.Vec4 }

type alias Uniforms = { perspective : Math.Matrix4.Mat4, rotation : Math.Matrix4.Mat4 }

type alias Varying = { vcolor : Math.Vector3.Vec3 }


triangle : (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> (Float,Float,Float,Float) -> (Vertex,Vertex,Vertex)
triangle a b c = 
    let normal = 
        crossNormalized a b c
        |> Math.Vector4.fromTuple
    in
    ( { pos = Math.Vector4.fromTuple a, normal = normal }
    , { pos = Math.Vector4.fromTuple b, normal = normal }
    , { pos = Math.Vector4.fromTuple c, normal = normal })

toMesh : Array (Vertex,Vertex,Vertex) -> Mesh
toMesh a = 
    let n = Array.length a // 21845 
        -- 21845 * 3 = 65535. WebGL maxes out at 65536 vertices. Since I am not reusing vertices, it maxes at 21845 triangles.
    in List.map 
        (\s -> 
            Array.slice 
                (21845*s) 
                (21845*(s+1)) 
                a
            |> Array.toList
            |> WebGL.triangles)
        (List.range 0 n)
