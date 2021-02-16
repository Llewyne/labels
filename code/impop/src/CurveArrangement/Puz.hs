{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module CurveArrangement.Puz where


import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry hiding (endPoints, head, init)
import qualified Data.CircularSeq as C

import Convert

import Algorithms.Geometry.Misc

import CurveArrangement.Types
import CurveArrangement.Basic

import PP.Puz


caToPuz :: (RealFrac r, Show r) => CA r -> [PuzObject]
caToPuz ca = map (edgeToPuz ca) (toList $ edges' ca) ++ map (faceToPuz ca . fst) (toList $ internalFaces ca)
  -- does order matter?	       
           



edgeToPuz :: (RealFrac r, Show r) => CA r -> Dart CAS -> PuzObject          
edgeToPuz ca = Path . bezierToPuz . edgeBezier ca

faceToPuz :: (RealFrac r, Show r) => CA r -> FaceId' CAS -> PuzObject          
faceToPuz ca i = makeDynamic $ beziersToPuz $ map (edgeBezier ca) $ toList $ outerBoundaryDarts i ca


 




