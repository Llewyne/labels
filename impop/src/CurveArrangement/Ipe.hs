{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module CurveArrangement.Ipe where


import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline hiding (endPoints)
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry hiding (endPoints, head, init)
import qualified Data.CircularSeq as C

import Convert

import Algorithms.Geometry.Misc

import CurveArrangement.Types
import CurveArrangement.Basic
import CurveArrangement.Construct
--import CurveArrangement.ForceDirectedStubs

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types
import Data.Geometry.Ipe.Color (named)
import Data.Colour.SRGB (RGB(..))

import Nonogram.PathType

{- IPE PAGE TO CURVE ARRANGEMENT -}

-- | Turn an ipe page containing curves into a CA r. Red curves are fixed.
--   Will detect intersections and subdivide curves if necessary.
fromIpePage :: (Ord r, RealFrac r, Show r) => IpePage r -> CA r
fromIpePage = construct . convertPathsToBeziers . pagePaths

-- | Faster version of fromIpe, assumes curves do not intersect.
fromIpePlanar :: (Ord r, RealFrac r, Show r) => IpePage r -> CA r
fromIpePlanar = constructPlanar . convertPathsToBeziers . pagePaths

{- PAGE TO PATHS, SEPARATING PATHS -}

-- | Get the paths out of a page.
pagePaths :: (Eq r, Fractional r, Show r) => IpePage r -> [Path r :+ NonoPathType]
pagePaths = separatePaths . concatMap objectPaths . view content 

-- | Get the paths out of an object.
objectPaths :: IpeObject r -> [Path r :+ IpeAttributes Path r]
objectPaths (IpeGroup (Group os :+ _)) = concatMap objectPaths os
objectPaths (IpePath  (p        :+ a)) = [p :+ a]
objectPaths _                          = []

-- should check somewhere if there are other important attributes, especially if they change the geometry?


-- | Separate paths into frame, boundary paths and free paths based on color.
separatePaths :: (Eq r, Fractional r) => [Path r :+ IpeAttributes Path r] -> [Path r :+ NonoPathType]
separatePaths ps = map (\p -> _core p :+ NonoPathType (isFrame p) (isBoundary p)) ps

-- test on red and blue channel separately?
isFrame :: (Eq r, Fractional r) => Path r :+ IpeAttributes Path r -> Bool
isFrame p =  p^.extra.ixAttr SStroke == Just (named "blue")
          || p^.extra.ixAttr SStroke == Just (named "purple")
          || p^.extra.ixAttr SStroke == Just (IpeColor . Valued $ RGB 0 0 1)

isBoundary :: (Eq r, Fractional r) => Path r :+ IpeAttributes Path r -> Bool
isBoundary p =  p^.extra.ixAttr SStroke == Just (named "red")
             || p^.extra.ixAttr SStroke == Just (named "purple")
             || p^.extra.ixAttr SStroke == Just (IpeColor . Valued $ RGB 1 0 0)
  

{-}
redPaths :: [Path Float :+ IpeAttributes Path Float] -> [Path Float :+ IpeAttributes Path Float]
redPaths = filter (\p -> p^.extra.ixAttr SStroke == Just red)
  where red = named "red" :: IpeColor Float
-}


{- CONVERT PATHS TO CURVES -}

-- convert paths to list of disconnected curves

convertPathsToBeziers :: Fractional r => [Path r :+ e] -> [BezierSpline 3 2 r :+ e]
convertPathsToBeziers = concatMap (\(p :+ e) -> map (:+ e) $ convertPathToBeziers p)

convertPathToBeziers :: Fractional r => Path r -> [BezierSpline 3 2 r]
convertPathToBeziers = concatMap broe . toList . view pathSegments

broe :: Fractional r => PathSegment r -> [BezierSpline 3 2 r]
broe (PolyLineSegment        (p)) = map segmentToCubic $ polyLineSegments p
broe (PolygonPath           (sp)) = map segmentToCubic $ listEdges sp
broe (CubicBezierSegment     (b)) = [b]
broe (QuadraticBezierSegment (b)) = [quadraticToCubic b]
broe _ = []
-- need to do something with curves!


-- some functions for extracting separete line segments:

polyLineSegments :: PolyLine d p r -> [LineSegment d p r]
polyLineSegments pl = zipWith pointsToSeg (init $ toList $ pl ^. points) (tail $ toList $ pl ^. points)

pointsToSeg :: Point d r :+ p -> Point d r :+ p -> LineSegment d p r
pointsToSeg p q = LineSegment (Closed p) (Closed q)

-- listEdges :: SimplePolygon p r -> [LineSegment 2 p r]

segmentToCubic :: (Fractional r) => LineSegment 2 _ r -> BezierSpline 3 2 r
segmentToCubic s = let [p1 :+ _, p4 :+ _] = s ^.. endPoints
                       p2 = p1 .+^ (1/3) *^ (p4 .-. p1)
                       p3 = p1 .+^ (2/3) *^ (p4 .-. p1)
                   in Bezier3 p1 p2 p3 p4

quadraticToCubic :: (Fractional r) => BezierSpline 2 2 r -> BezierSpline 3 2 r
quadraticToCubic q = let [p1, m, p4] = q ^. controlPoints . to toList
                         p2 = p1 .+^ (2/3) *^ (m .-. p1)
                         p3 = p4 .+^ (2/3) *^ (m .-. p4)
                     in Bezier3 p1 p2 p3 p4




{- CURVE ARRANGEMENT TO IPE PAGE -}


toIpePage :: RealFrac r => CA r -> IpePage r
toIpePage = makePage . caToIpePaths


caToIpePaths :: RealFrac r => CA r -> [Path r :+ IpeAttributes Path r]
caToIpePaths = map makePath . extractCurves

-- extract segments -- Beziers?
extractCurves :: RealFrac r => CA r -> [BezierSpline 3 2 r :+ NonoPathType]
extractCurves ca = map (\i -> edgeBezier ca i :+ nonoPathType ca i) (toList $ edges' ca)

nonoPathType :: CA r -> Dart CAS -> NonoPathType
nonoPathType ca i = 
  let frameEdge = leftFace i ca == outerFaceId ca || rightFace i ca == outerFaceId ca
      picEdge   = _full (ca ^. dataOf (leftFace i ca)) /= _full (ca ^. dataOf (rightFace i ca))
      frozen    = _froz $ ca ^. dataOf i -- not actully needed?
  in NonoPathType frameEdge picEdge

-- turn segments into paths
makePath :: BezierSpline 3 2 r :+ NonoPathType -> Path r :+ IpeAttributes Path r
makePath (b :+ t) = ipeBezier b & extra . ixAttr SStroke .~ Just (named $ pathColor t)

--pathColor :: NonoPathType -> String
pathColor t |     (_framePath t) && not (_boundaryPath t) = "blue"
            | not (_framePath t) &&     (_boundaryPath t) = "red"
            |     (_framePath t) &&     (_boundaryPath t) = "purple"
            | otherwise                                   = "black"

-- create page out of paths
makePage :: [Path r :+ IpeAttributes Path r] -> IpePage r
makePage ps = fromContent $ map iO ps

