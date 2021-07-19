{-# LANGUAGE OverloadedStrings #-}

module TestLabelPipeline where

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
import CurveArrangement.Loader

import Misc.Time
import Misc.Ipe

import Nonogram
import Nonogram.Puz
import Nonogram.Ipe
import Nonogram.PathType


import Misc.SpanningTree

import Eva.SATLabeling (placeLabelsSAT)
import Eva.DynamicLabeling (placeLabelsDynamic)
import Eva.SATLabelingExtensible (placeLabelsSATExtensible)
import Data.Geometry.Vector
import Eva.Util

import Data.List

import Debug.Trace

debug_log = False

main :: IO ()
main = do
  putStrLn "enter file name (without extension)"
  name <- getLine
  labelTest name

debug x y | debug_log = flip trace x y
        | otherwise = x
-- If two ports of the same label have the same position, or if the directions don't match then the label is not valid
validateLabel :: UnplacedLabel -> Bool
validateLabel (ps,_) = length(nub points) == length points && vectorsInverse ps
  where 
    points = map _location ps
    vectorsInverse ((Port _ v1 _):(Port _ v2 _):_) = (a < 0.5 || isNaN a) `debug` ((show (negated v1)++(show v2)++ "::" ++ (show a) ++ (show $ a < 0.02)))
      where a = abs((angleBetweenVectors (negated v1) v2))

labelTest :: String -> IO ()
labelTest name = do

  -- get the current time
  start <- time
  since start

  -- first, read in a curve arrangement from an ipe file
  car <- loadCAR $ "ipe/" ++ name
  putStrLn $ "curve arrangement:" 
  writeFile "log/car.txt" $ show car
  since start

  -- make a DFS tree of the faces of the arrangement
  let tre = faceTree car
  putStrLn $ "depth first tree: " ++ show (length tre) 
  writeFile "log/tree.txt" $ show tre
  since start

  -- find out which cells of the arrangement should be filled
  let caf = fillFaces car
  putStrLn $ "curve arrangement with solution:" 
  writeFile "log/caf.txt" $ show caf
  since start

  -- extract a list of pointers to faces that form solution
  let sol = fullFaces caf
  putStrLn $ "solution faces: " ++ show (length sol)
  writeFile "log/solution.txt" $ unlines $ map show sol
  since start

  -- collect clues in unplaced labels
  let upl = determineClues caf sol
  putStrLn $ "unplaced labels: " ++ show (length upl)  
  --writeFile "log/unlabels.txt" $ unlines $ map show upl
  since start

 

    -- filter out invalid labels
  let fupl =   nub $ filter validateLabel upl
  putStrLn $ "filtered unplaced labels: " ++ show (length fupl)  
  writeFile "log/funlabels.txt" $ unlines $ map show fupl
  since start

  -- build the frame
  let frame = extractFrame caf
  putStrLn $ "frame: " ++ show (length $ polygonVertices frame) 
  writeFile "log/frame.txt" $ show frame
  since start

  -- place the labels
  pl  <- placeLabelsSATExtensible fupl frame -- frame
  putStrLn $ "labels: " ++ show (length pl)
  writeFile "log/labels.txt" $ unlines $ map show pl
  since start

  -- construct a nonogram
  let non = Nonogram frame caf pl
  putStrLn $ "nonogram: "
  writeFile "log/nonogram.txt" $ show non
  since start

  -- output the nonogram as puzcode
  let puz = toPuzCode non
  putStrLn $ "puzcode:"
  writeFile ("puz/" ++ name ++ ".puz") puz
  since start

  -- output the nonogram as ipe file
  let ipe = toIpe non
  putStrLn $ "ipe:"
  writeIpePage ("ipe/" ++ name ++ ".labeled.ipe") ipe
  since start


  
  return ()

