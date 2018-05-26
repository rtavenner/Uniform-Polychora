module Convert exposing (convert)

import Matrix4
import Math.Matrix4

convert : Matrix4.Float4x4 -> Math.Matrix4.Mat4
convert m =
    case Math.Matrix4.makeFromList (Matrix4.foldr (::) [] (Matrix4.transpose m)) of
        Nothing -> Debug.crash "This is not possible. The foldr creates a 16-element list, and the makeFromList needs one."
        Just m2 -> m2
