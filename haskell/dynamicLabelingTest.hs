{-# LANGUAGE OverloadedStrings #-}

module DynamicLabelingTest where

import Control.Lens
import Data.Ext
import qualified Data.List.NonEmpty as NonEmpty

import Data.Geometry.Ipe

import Data.Geometry.Point
import Data.Geometry.BezierSpline hiding (snap)

import Data.Geometry.Polygon
import Data.Geometry.PlanarSubdivision
import Data.Geometry.PlanarSubdivision.More


import Algorithms.Geometry.Misc

import Algorithms.Geometry.EuclideanMST.EuclideanMST

import CurveArrangement
import CurveArrangement.Types
import CurveArrangement.Construct
import CurveArrangement.Ipe
import CurveArrangement.Puz

import Misc.Time
import Misc.Ipe

import Nonogram
import Nonogram.Puz
import Nonogram.Ipe
import Nonogram.PathType


import Misc.SpanningTree

import Labeling
import Tests

labelTest :: IO ()
labelTest = do

  -- get the current time
  start <- time
  since start

  -- build the frame
  let frame = fromPoints [Point2 0 0, Point2 71 0, Point2 71 180, Point2 0 180]
  putStrLn $ "frame: " ++ show (length $ polygonVertices frame) 
  writeFile "log/frame.txt" $ show frame
  since start

  -- place the labels
  let pl  = placeLabelsDynamic 0 1 ((length sideTest1)-1) 1 (parseLabel sideTest1)
  putStrLn $ "labels: " ++ show (length pl) 
  writeFile "log/labels.txt" $ unlines $ map show pl
  since start

  -- construct a nonogram
  let non = Nonogram frame vierkant pl
  putStrLn $ "nonogram: "
  writeFile "log/nonogram.txt" $ show non
  since start

  -- output the nonogram as ipe file
  let ipe = toIpe non
  putStrLn $ "ipe:"
  writeIpePage ("ipe/" ++ name ++ ".labeled.ipe") ipe
  since start

  -- output the nonogram as puzcode
  let puz = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile ("puz/" ++ name ++ ".puz") puz
  since start

  
  return ()

