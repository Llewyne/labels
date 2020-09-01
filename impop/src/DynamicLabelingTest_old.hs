{-# LANGUAGE OverloadedStrings #-}

module DynamicLabelingTest where

import Control.Lens
import Data.Default
import Data.Ext
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.CircularSeq as C
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
  let frm = SimplePolygon . C.fromList . map (:+ def) $ [Point2 0 0, Point2 71 0, Point2 71 180, Point2 0 180]
  putStrLn $ "frame: " ++ show (length $ polygonVertices frm) 
  writeFile "log/frame.txt" $ show frm
  since start

  -- place the labels
  let pl  = placeLabelsDynamic 0 1 5 1 (map parseLabel sidetest1)
  putStrLn $ "labels: " ++ show (length pl) 
  writeFile "log/labels.txt" $ unlines $ map show pl
  since start

  --interior
  let itr = CurveArrangement.frame (SimplePolygon . C.fromList . map (:+ def) $ [Point2 5 5, Point2 10 5, Point2 10 10, Point2 5 10])

  -- construct a nonogram
  let non = Nonogram frm itr pl
  putStrLn $ "nonogram: "
  writeFile "log/nonogram.txt" $ show non
  since start

  -- output the nonogram as ipe file
  let ipe = toIpe non
  putStrLn $ "ipe:"
  writeIpePage ("ipe/dynamic.labeled.ipe") ipe
  since start

  -- output the nonogram as puzcode
  let puz = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile ("puz/dynamic.puz") puz
  since start

  
  return ()

