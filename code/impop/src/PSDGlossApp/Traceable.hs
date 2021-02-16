module PSDGlossApp.Traceable where

import Debug.Trace

import Control.Lens hiding (holesOf)

import Data.Ext
import Data.Foldable (toList)

import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, arc)

import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.More

import PSDGlossApp.Common
import PSDGlossApp.Selectable

{-
tracing :: (HasPSD v e f r s a, Hoverable s a) => GlossApp a -> GlossApp a
tracing = undefined -- output stuff to stdout
-}

traceable :: (Show v, Show e, Show f, Show r, RealFrac r) => Behaviour s v e f r
traceable = handleEvent %~~ handleTrace
  where
    handleTrace (EventMotion (x, y)) s =
      let t = _hoverChanged s
          h = _hover s
      in if not t then s else trace (info (_subdivision s) h) s {_hoverChanged = False}
    handleTrace _ s = s

info :: (Num r, Ord r, Show r, Show v, Show e, Show f) => PlanarSubdivision s v e f r -> PSE s -> String
info _ N = ""
info psd (V i) = "VERTEX " ++ show i ++ "\n"
              ++ "Data: " ++ show (psd ^. dataOf i) ++ "\n"
              ++ "Location: " ++ show (psd ^. locationOf i) ++ "\n"
              ++ "Degree: " ++ show (degree psd i) ++ "\n"
              ++ "Incident edges: " ++ show (incidentEdges i psd) ++ "\n"
              ++ "Adjacent vertices: " ++ show (neighboursOf i psd) ++ "\n"
info psd (D i) = "EDGE " ++ show i ++ "\n"
              ++ "Data: " ++ show (psd ^. dataOf i) ++ "\n"
              ++ "Twin: " ++ show (twin i) ++ "\n"
              ++ "Twin data: " ++ show (psd ^. dataOf (twin i)) ++ "\n"
              ++ "nextIncidentEdge: " ++ show (nextIncidentEdge i psd) ++ "\n"
              ++ "Next on curve: " ++ show (oppositeIncidentEdge psd i) ++ "\n"
              ++ "Incident vertices: " ++ show (endPoints i psd) ++ "\n"
              ++ "Incident faces: " ++ show (leftFace i psd, rightFace i psd) ++ "\n"
info psd (F i) = "FACE " ++ show i ++ "\n"
              ++ "Data: " ++ show (psd ^. dataOf i) ++ "\n"
              ++ "Outer: " ++ show (outerFaceId psd == i) ++ "\n"
              ++ "Boundary: " ++ show (boundaryVertices i psd) ++ "\n"
              ++ "Holes: " ++ show (toList $ holesOf i psd) ++ "\n"

-- ^ slowing down?


{- -- for tracing is the test whether something changed is important!
hoverFunc :: Float -> Float -> State -> State
hoverFunc x y st = 
  let l = locate (subdivision st) (Point2 x y)
  in (if l == hoverElement st then id else trace (info (subdivision st) l)) st {hoverElement = l}
-}