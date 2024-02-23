module Vector (
    Vector,
    Point,
    vX,
    vY,
    vZ,
    (+),
    (-),
    (*),
    (/),
    vDot,
    vInner,
    isInside,
    v2DZero,
    v3DZero,
    v2DId,
    v3DId,
    vZero,
    vId,
) where

import Data.Bool (Bool)
import Data.Ratio (Rational)
import qualified Prelude as P

-- Here, we use Rational as basic type to save data, in order to avoid precision lost.
data Vector = Vector2D P.Rational P.Rational | Vector3D P.Rational P.Rational P.Rational -- x,y/x,y,z
data Point = Point2D P.Rational P.Rational | Point3D P.Rational P.Rational P.Rational

vX :: Vector -> Rational
vX (Vector2D x _) = x
vX (Vector3D x _ _) = x

vY :: Vector -> Rational
vY (Vector2D _ y) = y
vY (Vector3D _ y _) = y

vZ :: Vector -> Rational
vZ (Vector3D _ _ z) = z
vZ _ = P.error "Vector 2D don't have Z Position."

v2DZero :: Vector
v2DZero = Vector2D 0 0

v3DZero :: Vector
v3DZero = Vector3D 0 0 0

vZero :: Vector -> Vector
vZero (Vector2D{}) = v2DZero
vZero (Vector3D{}) = v3DZero

v2DId :: Vector
v2DId = Vector2D 1 1

v3DId :: Vector
v3DId = Vector3D 1 1 1

vId :: Vector -> Vector
vId (Vector2D{}) = v2DId
vId (Vector3D{}) = v3DId

vAdd :: Vector -> Vector -> Vector
vAdd (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 P.+ x2) (y1 P.+ y2)
vAdd (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1 P.+ x2) (y1 P.+ y2) (z1 P.+ z2)
vAdd _ _ = P.error "There's no binary operator between Vector2D and Vector3D"

vSub :: Vector -> Vector -> Vector
vSub (Vector2D x1 y1) (Vector2D x2 y2) = Vector2D (x1 P.- x2) (y1 P.- y2)
vSub (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = Vector3D (x1 P.- x2) (y1 P.- y2) (z1 P.- z2)
vSub _ _ = P.error "There's no binary operator between Vector2D and Vector3D"

vMul :: Vector -> P.Rational -> Vector
vMul (Vector2D x y) a = Vector2D (x P.* a) (y P.* a)
vMul (Vector3D x y z) a = Vector3D (x P.* a) (y P.* a) (z P.* a)

vDiv :: Vector -> P.Rational -> Vector
vDiv (Vector2D x y) a = Vector2D (x P./ a) (y P./ a)
vDiv (Vector3D x y z) a = Vector3D (x P./ a) (y P./ a) (z P./ a)

vDot :: Vector -> Vector -> P.Rational
vDot (Vector2D x1 y1) (Vector2D x2 y2) = x1 P.* x2 P.+ y1 P.* y2
vDot (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) = x1 P.* x2 P.+ y1 P.* y2 P.+ z1 P.* z2
vDot _ _ = P.error "There's no binary operator between Vector2D and Vector3D"

vInner :: Vector -> Vector -> Vector
vInner (Vector3D a1 a2 a3) (Vector3D b1 b2 b3) = Vector3D (a2 P.* b3 P.- a3 P.* b2) (a3 P.* b1 P.- a1 P.* b3) (a1 P.* b2 P.- a2 P.* b1)
vInner (Vector2D a1 a2) (Vector2D b1 b2) = vInner (Vector3D a1 a2 0) (Vector3D b1 b2 0)
vInner _ _ = P.error "There's no binary operator between Vector2D and Vector3D"

(+) :: Vector -> Vector -> Vector
a + b = a `vAdd` b

(-) :: Vector -> Vector -> Vector
a - b = a `vSub` b

(*) :: Vector -> P.Rational -> Vector
a * b = a `vMul` b

(/) :: Vector -> P.Rational -> Vector
a / b = a `vDiv` b

isInside :: Point -> Point -> Point -> Bool
isInside (Point2D x1 y1) (Point2D x2 y2) (Point2D x y) = ((x1 P.<= x) P.&& (x P.<= x2)) P.&& ((y1 P.<= y) P.&& (y P.<= y2))
isInside (Point3D x1 y1 z1) (Point3D x2 y2 z2) (Point3D x y z) =
    ((x1 P.<= x) P.&& (x P.<= x2))
        P.&& ( (y1 P.<= y)
                P.&& (y P.<= y2)
                P.&& ((z1 P.<= z) P.&& (z P.<= z2))
             )
isInside _ _ _ = P.error "There's no operation."