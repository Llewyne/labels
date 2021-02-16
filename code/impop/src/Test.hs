{-# LANGUAGE OverloadedStrings, DataKinds, TypeOperators #-}

module Test where

import Control.Lens
import Data.Ext
import qualified Data.List.NonEmpty as NonEmpty

import Data.Default

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types
import Data.Geometry.Ipe.Color (named)

import Data.Geometry.Point
import Data.Geometry.BezierSpline hiding (snap)

import Data.Geometry.PlanarSubdivision.More

import Algorithms.Geometry.Misc
import Algorithms.Geometry.Snap

import Algorithms.Geometry.EuclideanMST.EuclideanMST

import CurveArrangement.Types
import CurveArrangement.Construct
import CurveArrangement.Ipe
import CurveArrangement.Puz
import CurveArrangement.Loader

import Misc.Time
import Misc.Ipe

import Misc.SpanningTree

import Nonogram
import Nonogram.Puz


{-
redPaths :: [Path Float :+ IpeAttributes Path Float] -> [Path Float :+ IpeAttributes Path Float]
redPaths = filter (\p -> p^.extra.attrLens SStroke == Just red)
  where red = named "red" :: IpeColor Float
-}





testcurve1, testcurve2, testcurve3, testcurve4 :: BezierSpline 3 2 Float
testcurve1 = Bezier3 (Point2 (-250) (-250)) (Point2 (-100) (100)) (Point2 (100) (-100)) (Point2 (250) (250))
testcurve2 = Bezier3 (Point2 (250) (-250)) (Point2 (-50) (100)) (Point2 (50) (-150)) (Point2 (-250) (250))
testcurve3 = Bezier3 (Point2 (-128) (128)) (Point2 (48) (80)) (Point2 (-16) (-48)) (Point2 (96) (-64))
testcurve4 = Bezier3 (Point2 (-64) (-80)) (Point2 (-48) (32)) (Point2 (48) (48)) (Point2 (64) (96))

testcurves = [testcurve1, testcurve2, testcurve3, testcurve4]


{-
makePlanar :: String -> IO ()
makePlanar name = do
  let input  = "ipe/" ++ name ++ ".ipe"
      output = "ipe/" ++ name ++ ".planar.ipe"
  page <- readIpePage input :: IO (IpePage Rational)
  let car = fromIpePage page
  writeIpePage output $ toIpePage car
-}

ipeOutputCurvesWithPointSet :: [BezierSpline 3 2 Rational] -> [Point 2 Rational] -> IpePage Rational
ipeOutputCurvesWithPointSet curves points =
  let paths = map makePath $ map (:+ def) curves 
      marks = map ipeDiskMark points
  in fromContent $ map iO paths ++ map iO marks
  -- turn segments into paths


readPlanar :: String -> IO (CA Rational)
readPlanar name = do
  let input = "ipe/" ++ name ++ ".planar.ipe"
  page <- readIpePage input
  return $ fromIpePlanar page

writePuz :: String -> CA Rational -> IO ()
writePuz name car = do
  let output = "puz/" ++ name ++ ".puz"
--      sol = map toEnum [5, 10, 11, 13, 17, 18, 23]
      sol = map toEnum [1, 2, 3, 4, 6, 8, 10]
      upl = determineClues car sol
      pl  = placeLabels undefined upl  
      non = Nonogram undefined car pl
      cod = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile output cod
