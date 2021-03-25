module Eva.Util where

import Control.Lens
import Data.List hiding (intersect)
import Data.List.Split
import Data.Ord
import Data.Vinyl hiding (Label)
import Data.Vinyl.CoRec
import Data.Ext
import Data.Geometry hiding (head,direction,init,replicate,unit)
import Data.Geometry.PlanarSubdivision hiding(location)
import Data.Geometry.Transformation
import Data.Geometry.Matrix

-- convert a line segment to a line
toLine :: LineSegment 2 () Float -> Line 2 Float
toLine ls = lineThrough (ls^.start.core) (ls^.end.core)


-- create a line segment that represents a leader, from a given starting position, direction and length
leader :: Point 2 Float -> Vector 2 Float -> Int -> LineSegment 2 () Float
leader p v l =  ClosedLineSegment (p :+ ()) (q :+ ())
    where
    q = p .+^ ((signorm v)^*(fromIntegral l))

inverseVector :: Vector 2 Float -> Vector 2 Float
inverseVector v = Vector2 (v^.yComponent) (v^.xComponent)

-- intersection point
-- y = m1*x + b1
-- y = m2*x + b2
-- m1*x + b1 = m2*x + b2
-- m1*x - m2*x = b2 - b1
-- (m1-m2)*x = b2 - b1
-- x = (b2-b1)/(m1-m2) 
-- y = m1*x + b1

-- Intersection point line l1:m1*x + b1 and l2:m2*x + b2
lineIntersection :: Float -> Float -> Float -> Float -> (Float,Float)
lineIntersection m1 b1 m2 b2 = (x,m1*x + b1)
    where
        x = (b2-b1)/(m1-m2)


--interpolate the y coordinates of a line segment
inty ls = OpenLineSegment (Point2 x1 y :+ ()) (Point2 x2 y :+ ())
    where 
        x1 = ls^.start.core.xCoord
        x2 = ls^.end.core.xCoord
        y1 = ls^.start.core.yCoord
        y2 = ls^.end.core.yCoord
        y = (y1+y2) / 2

lineSegmentDirection :: LineSegment 2 () Float -> Vector 2 Float
lineSegmentDirection ls = signorm (Vector2 ((ls^.end.core.xCoord) - (ls^.start.core.xCoord)) ((ls^.end.core.yCoord) - (ls^.start.core.yCoord)))


rotationMatrix :: Float -> Transformation 2 Float
rotationMatrix a = Transformation . Matrix $ Vector3 (Vector3 (cos a)       (sin a) 0)
                                                     (Vector3 (-(sin a))    (cos a) 0)
                                                     (Vector3 0             0       1)

-- Transformation matrix for a new basis defined by a point and a vector (direction of new x-axis)
transformOrigin :: Point 2 Float -> Vector 2 Float -> Transformation 2 Float
transformOrigin p v = transformOriginV v |.| transformOriginP p

transformOriginP :: Point 2 Float -> Transformation 2 Float
transformOriginP p = Transformation . Matrix $ Vector3  (Vector3 1 0 (-p^.xCoord))
                                                        (Vector3 0 1 (-p^.yCoord))
                                                        (Vector3 0 0 1)

transformOriginV :: Vector 2 Float -> Transformation 2 Float
transformOriginV v = Transformation . Matrix $ Vector3 (Vector3 (-v^.xComponent) (-v^.yComponent) 0)
                                                        (Vector3 (v^.yComponent) (-v^.xComponent)  0)
                                                        (Vector3 0          0           1)

toVectorBase :: LineSegment 2 () Float -> Vector 2 Float
toVectorBase ls = signorm (ls^.end.core .-. ls^.start.core)

-- Transformation for translation to the origin
toBaseTransformation :: LineSegment 2 () Float -> Transformation 2 Float
toBaseTransformation ls = transformOrigin (ls^.end.core) (toVectorBase ls)