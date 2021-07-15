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

-- debug_log = False


-- debug x y | debug_log = flip trace x y
--         | otherwise = x

roundSegment :: LineSegment 2 () Float -> LineSegment 2 () Float
roundSegment ls = ClosedLineSegment ((roundPoint $ ls^.start.core) :+ ()) ((roundPoint $ ls^.end.core) :+ ())

roundPoint :: Point 2 Float -> Point 2 Float
roundPoint p = Point2 ((fromIntegral $ round $ p^.xCoord)::Float) ((fromIntegral $ round $ p^.yCoord)::Float)

-- simplify a list of line segments by merging parallel lines
simplify 
    :: [LineSegment 2 () Float] -- List of line segments
    -> [LineSegment 2 () Float] -- Simplified list of line segments
simplify ls
    | length ll == 0 = [l]
    | abs (abs(getM l) - abs(getM (last ll))) < 0.01 || getM l == getM (last ll) = (((last ll)&start .~ (last ll)^.start)&end .~ l^.end) : init ll
    | otherwise = l:ll
    where (l:ll) = simplify_ ls

simplify_ :: [LineSegment 2 () Float] -> [LineSegment 2 () Float]
simplify_ [l] = [l]
simplify_ (l:ll:ls)
    | abs (abs(getM l) - abs(getM ll)) < 0.01 || getM l == getM ll = simplify_ (((l&start .~ l^.start)&end .~ ll^.end) : ls)
    | otherwise = l:simplify_ (ll:ls)

removeZeroLength :: [LineSegment 2 () Float] -> [LineSegment 2 () Float]
removeZeroLength = filter (\x->x^.start.core /= x^.end.core)

-- get the slope of a line segment
getM :: LineSegment 2 () Float -> Float
getM l = (l^.end.core.yCoord - l^.start.core.yCoord) / (l^.end.core.xCoord - l^.start.core.xCoord)

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

lineFromVectorPoint :: Vector 2 Float -> Point 2 Float -> Line 2 Float
lineFromVectorPoint v p = lineThrough p (p .+^v)

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
rotationMatrix a = Transformation . Matrix $ Vector3 (Vector3 (cos a)       (-(sin a)) 0)
                                                     (Vector3 (sin a)    (cos a) 0)
                                                     (Vector3 0             0       1)

-- Transformation matrix for a new basis defined by a point and a vector (direction of new x-axis)
transformOrigin :: Point 2 Float -> Vector 2 Float -> Transformation 2 Float
transformOrigin p v = transformOriginV v |.| transformOriginP p

transformOriginP :: Point 2 Float -> Transformation 2 Float
transformOriginP p = Transformation . Matrix $ Vector3  (Vector3 1 0 (-p^.xCoord))
                                                        (Vector3 0 1 (-p^.yCoord))
                                                        (Vector3 0 0 1)

transformOriginV :: Vector 2 Float -> Transformation 2 Float
transformOriginV v = Transformation . Matrix $ Vector3 (Vector3 (v^.xComponent) (v^.yComponent) 0)
                                                        (Vector3 (-v^.yComponent) (v^.xComponent)  0)
                                                        (Vector3 0          0           1)

toVectorBase :: LineSegment 2 () Float -> Vector 2 Float
toVectorBase ls = signorm (ls^.start.core .-. ls^.end.core)

-- Transformation for translation to the origin
toBaseTransformation :: LineSegment 2 () Float -> Transformation 2 Float
toBaseTransformation ls = transformOrigin (ls^.end.core) (toVectorBase ls)

angleBetweenVectors :: Vector 2 Float -> Vector 2 Float -> Float
angleBetweenVectors v1 v2 = (angleVector v2) - (angleVector v1)

angleVector :: Vector 2 Float -> Float
angleVector v = atan (((v^.yComponent)::Float) / ((v^.xComponent)::Float))

magnitude :: Vector 2 Float -> Float
magnitude v = sqrt (v^.xComponent*v^.xComponent + v^.yComponent*v^.yComponent)