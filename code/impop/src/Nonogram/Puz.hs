{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Nonogram.Puz where


import Control.Lens
import Data.Default
import Data.Foldable (toList)
import Data.Ext

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init)
import Data.Geometry.BezierSpline hiding (reverse)
import Data.Geometry.LineSegment
import Data.Geometry.PlanarSubdivision hiding (endPoints)

import Data.Geometry hiding (endPoints, head, init, _direction)
import qualified Data.CircularSeq as C

import Convert

import Algorithms.Geometry.Misc

import CurveArrangement.Types
import CurveArrangement.Puz

import Nonogram

import PP.Puz

toPuzCode :: Nonogram -> String
toPuzCode nono = (unlines $ map code $ caToPuz $ _interior nono)
              ++ (unlines $ map code $ lbToPuz $ _labels nono)



lbToPuz :: [Label] -> [PuzObject]
lbToPuz = map labelToPuz

labelToPuz :: Label -> PuzObject
labelToPuz l = let p = _location  $ _port l
                   v = _direction $ _port l
                   d = _offset l
               in Compound $ Path (pointsToPuzPath [p, p .+^ d *^ v])
                           : map (\(i, c) -> translate (p .-. origin 
                                                          ^+^ (d + fromInteger i * res) *^ v
                                                          ^+^ (0.5 * res) *^ v
                                                          ^+^ (0.5 * res) *^ if _side $ _port l
                                                                             then rot90 $ rot90 $ rot90 v
                                                                             else rot90 v
                                                       )
                                                       (clueBox v c)
                                 )                     
                                 (zip [0 .. ] $ reverse $ _clue l)
               -- correct order? needs flipped? encode where?

res :: Float
res = 16

clueBox :: Vector 2 Float -> Int -> PuzObject
clueBox d c = Compound [Text $ show c, Path $ box d]
  -- draw square around text, oriented by d

box :: Vector 2 Float -> PuzPath 
box v = pointsToPuzPath [ origin .+^ 0.5 * res *^ v .+^ 0.5 * res *^ rot90 v
                        , origin .+^ 0.5 * res *^ v .-^ 0.5 * res *^ rot90 v
                        , origin .-^ 0.5 * res *^ v .-^ 0.5 * res *^ rot90 v
                        , origin .-^ 0.5 * res *^ v .+^ 0.5 * res *^ rot90 v
                        , origin .+^ 0.5 * res *^ v .+^ 0.5 * res *^ rot90 v
                        ]


rot90 :: Num r => Vector 2 r -> Vector 2 r
rot90 v = let [x, y] = toList v
          in Vector2 (-y) x


