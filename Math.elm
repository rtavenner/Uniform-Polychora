module Math exposing (..)

import Vector4 exposing (..)

projectAlong : Float4 -> Float4 -> Float4
projectAlong v =
    \u -> sub u (scale (dot u v / dot v v) v)

gramSchmidt : List Float4 -> Vec4 Float4
gramSchmidt =
    let help done rest = 
            case rest of
                [] -> done
                u::us ->
                    let v = List.foldl projectAlong u done
                    in if lengthSquared v <= 1e-6
                    then help done us
                    else help (v :: done) us
    in \l -> 
        help [] (l ++ [(1,0,0,0),(0,1,0,0),(0,0,1,0),(0,0,0,1)])
        |> List.map normalize
        |> \l -> case l of
            [u1,u2,u3,u4] -> (u1,u2,u3,u4)
            _ -> Debug.crash "Not possible, due to details of Gram-Schmidt algorithm."

crossNormalized : Float4 -> Float4 -> Float4 -> Float4
crossNormalized u v w = 
    gramSchmidt [u,v,w] |> \(x,_,_,_) -> x

perps : Float4 -> Float4 -> (Float4, Float4)
perps u v = 
    gramSchmidt [u,v] |> \(u2,v2,_,_) -> (u2,v2)

