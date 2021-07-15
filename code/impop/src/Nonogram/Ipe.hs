{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Nonogram.Ipe where


import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext
import Data.Text (pack)

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline hiding (reverse)
import Data.Geometry.LineSegment
import Data.Geometry.PolyLine
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry hiding (endPoints, head, init, _direction)
import qualified Data.CircularSeq as C


import Algorithms.Geometry.Misc

import CurveArrangement.Types
import CurveArrangement.Ipe

import Nonogram

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types

toIpe :: Nonogram -> IpePage Float
toIpe nono =
  let intObs = map (fmap realToFrac) $ map iO $ caToIpePaths $ _interior nono
      labObs = concatMap labelToIpe $ _labels nono
  in fromContent $ intObs ++ labObs





labelToIpe :: Label -> [IpeObject Float]
labelToIpe l = let p = _location  $ _port l
                   v = _direction $ _port l
                   d = _offset l
               in  iO (ipePolyLine $ polyLine [p, p .+^ d *^ v])
                 : map (\(i, c) -> clueBox (p .+^ (d + fromInteger i * res) *^ v
                                              .+^ (0.5 * res) *^ v
                                              .+^ (0.5 * res) *^ if _side $ _port l
                                                                 then rot90 $ rot90 $ rot90 v
                                                                 else rot90 v
                                           ) v c
                       ) (zip [0 .. ] $ reverse $ _clue l)
               -- correct order? needs flipped? encode where?

res :: Float
res = 16

clueBox :: Point 2 Float -> Vector 2 Float -> Int -> IpeObject Float
clueBox p d c = iO $ ipeGroup [iO $ ipeLabel $ pack (show c) :+ p, box p d]
  -- draw square around text, oriented by d

box :: Point 2 Float -> Vector 2 Float -> IpeObject Float
box p v = iO $ ipePolyLine $ polyLine 
      [ p .+^ 0.5 * res *^ v .+^ 0.5 * res *^ rot90 v
      , p .+^ 0.5 * res *^ v .-^ 0.5 * res *^ rot90 v
      , p .-^ 0.5 * res *^ v .-^ 0.5 * res *^ rot90 v
      , p .-^ 0.5 * res *^ v .+^ 0.5 * res *^ rot90 v
      , p .+^ 0.5 * res *^ v .+^ 0.5 * res *^ rot90 v
      ]

polyLine :: [Point 2 r] -> PolyLine 2 () r
polyLine ps = let Just p = fromPoints $ map (:+ ()) ps in p

rot90 :: Num r => Vector 2 r -> Vector 2 r
rot90 v = let [x, y] = toList v
          in Vector2 (-y) x

translate :: Vector 2 r -> IpeObject Float -> IpeObject Float
translate = const id