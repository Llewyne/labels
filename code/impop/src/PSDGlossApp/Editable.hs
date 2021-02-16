module PSDGlossApp.Editable where

import Control.Lens

import Data.Default  
import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, arc)

import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.More
import Data.Geometry.PlanarSubdivision.Dynamic

import Algorithms.Geometry.Misc

import PSDGlossApp.Common
import PSDGlossApp.Selectable

editable :: (Default v, Default e, Show v, Show e, Show f, Show r, RealFrac r) => Behaviour s v e f r
editable = handleEvent %~~ handleEdit
  where
    handleEdit (EventKey (SpecialKey KeySpace) Down _ _) s = -- traceShow (selection s) $
      let l = _selection s
      in case l of [D i]      -> s & subdivision %~ (sub' i)
                   [V a, V b] -> s & subdivision %~ (add' a b)
                   _          -> s
    handleEdit _ s = s

sub' :: (Default v, Show v, Show e, Show f, Show r, RealFrac r) => Dart s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
sub' i d = let (a, b) = d & endPoints i
               p = average [d ^. locationOf a, d ^. locationOf b]
               v = def
               f x = (x, x)
           in splitEdge a b p v f d


add' :: (Default e, Show v, Show e, Show f, Show r) => VertexId' s -> VertexId' s -> PlanarSubdivision s v e f r -> PlanarSubdivision s v e f r
add' a b d | a == b = d
           | length (commonDarts d a b) > 0 = d
           | length (commonFaces d a b) == 0 = d
           | otherwise = let f x = (x, x)
                         in splitFace a b (def, def) f d


