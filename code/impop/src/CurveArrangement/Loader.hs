module CurveArrangement.Loader where

import System.FilePath
import System.Directory

import Data.Time.Clock

import Data.Geometry.Ipe
import Data.Geometry.Ipe.Types
--import Data.Geometry.Ipe.Color (named)

import Misc.Time
import Misc.Ipe

import CurveArrangement.Types
import CurveArrangement.Ipe
import CurveArrangement.Construct


import Data.Ext


import Data.Geometry.Point
import Data.Geometry.BezierSpline hiding (snap)

import Control.Monad.Extra
import Control.Lens


import Algorithms.Geometry.Snap

{-
import Data.List
import qualified Data.List.NonEmpty as NonEmpty
import Data.Tree
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Default
import Data.Foldable (toList)

import Data.Geometry.Point
import Data.Geometry.Vector hiding (head, init, last)
import Data.Geometry.LineSegment hiding (endPoints)
import Data.Geometry.PlanarSubdivision hiding (endPoints)
import Data.PlanarGraph.Dart 

import Data.Geometry.PlanarSubdivision.ForceDirected
import Data.Geometry.PlanarSubdivision.More

import Data.Geometry hiding (endPoints, head, init, _direction)
import qualified Data.CircularSeq as C

import Convert

import Algorithms.Geometry.EuclideanMST.EuclideanMST
import Algorithms.Geometry.Diameter.Naive
import Algorithms.Geometry.Misc


--import CurveArrangement.ForceDirectedStubs

import Nonogram.PathType


import Misc.SpanningTree
import Data.Tree



import Debug.Trace
-}

-- conclusion: it is ok to write and reload when necessary, just make it modular



-- main reader: looks for files that store intermediate products to speed up loading
-- but also checks if time stamps are chronological
loadCAR :: FilePath -> IO (CA Float)
loadCAR filePath = do
  makePlanar filePath
  makeSnapped filePath
--   fromSnapped filePath


-- read an arbitrary ipe file and convert it into a planar one (preserving properties)
-- might be more generally useful...
-- depends on error parameter?

makePlanar :: FilePath -> IO ()
makePlanar filePath =

  let base   = dropExtensions filePath
      input  = addExtension base ".ipe"
      output = addExtension base ".planar.ipe"
  in whenM (necessary input output) $ do

    putStrLn $ "makePlanar called on " ++ filePath

    -- get the current time
    start <- time
    since start

    -- read the ipe page
    page <- readIpePage input :: IO (IpePage Rational)
    let paths :: [Path Rational :+ IpeAttributes Path Rational]
        paths = concatMap objectPaths $ view content page
    putStrLn $ "page paths: " ++ show (length paths)
    writeFile "log/oldpaths.txt" $ unlines $ map show paths
    since start

    -- extract the contents of the page as a set of curves
    let curves :: [BezierSpline 3 2 Rational :+ IpeAttributes Path Rational]
        curves = convertPathsToBeziers paths
    putStrLn $ "converted curves: " ++ show (length curves)
    writeFile "log/converted.txt" $ unlines $ map show curves
    since start

    -- compute all intersection points between the curves
    let points :: [[Point 2 Rational]]
        points = intersectionPoints $ map _core curves
    putStrLn $ "intersection points: " ++ show ((sum $ map length points))
    writeFile "log/points.txt" $ unlines $ map show points
    since start

    -- chop the curves into smaller pieces so they no longer intersect
    let chopped :: [BezierSpline 3 2 Rational :+ IpeAttributes Path Rational]
        chopped = concat $ zipWith subdivide curves points
    putStrLn $ "chopped curves: " ++ show (length chopped)
    writeFile "log/chopped.txt" $ unlines $ map show chopped
    since start

    -- convert chopped Beziers to Ipe paths again
    -- should we attempt to glue them together into longer paths already here?
    let newpaths :: [Path Rational :+ IpeAttributes Path Rational]
        newpaths = map (\(b :+ t) -> ipeBezier b & set extra t) chopped
    putStrLn $ "new paths: " ++ show (length newpaths)
    writeFile "log/newpaths.txt" $ unlines $ map show newpaths
    since start

    -- write the ipe page
    let newpage = makePage newpaths
    writeIpePage output newpage
    since start


makeSnapped :: FilePath -> IO (CA Float)
makeSnapped filePath = do
    let base   = dropExtensions filePath
    let input  = addExtension base ".planar.ipe"
    let output = addExtension base ".snapped.ipe"

    putStrLn $ "makeSnapped called on " ++ filePath

    -- get the current time
    start <- time
    since start

    -- read the ipe page
    page <- readIpePage (addExtension base ".planar.ipe") :: IO (IpePage Float)
    let paths :: [Path Float :+ IpeAttributes Path Float]
        paths = concatMap objectPaths $ view content page
    putStrLn $ "page paths: " ++ show (length paths)
    writeFile "log/oldpaths.txt" $ unlines $ map show paths
    since start

    -- extract the contents of the page as a set of curves
    let curves :: [BezierSpline 3 2 Float :+ IpeAttributes Path Float]
        curves = convertPathsToBeziers paths
    putStrLn $ "converted curves: " ++ show (length curves)
    writeFile "log/converted.txt" $ unlines $ map show curves
    since start

    -- snap all curve endpoints that are close to each other
    let snapped :: [BezierSpline 3 2 Float :+ IpeAttributes Path Float]
        snapped = snap 0.01 curves
    putStrLn $ "snapped curves: " ++ show (length snapped)
    writeFile "log/snapped.txt" $ unlines $ map show snapped
    since start

    -- convert chopped Beziers to Ipe paths again
    -- should we attempt to glue them together into longer paths already here?
    let newpaths :: [Path Float :+ IpeAttributes Path Float]
        newpaths = map (\(b :+ t) -> ipeBezier b & set extra t) snapped
    putStrLn $ "new paths: " ++ show (length newpaths)
    writeFile "log/newpaths.txt" $ unlines $ map show newpaths
    since start

    let nonopaths = separatePaths newpaths

    -- extract the contents of the page as a set of curves
    let -- curves :: [BezierSpline 3 2 r :+ IpeAttributes Path r]
        curves = convertPathsToBeziers nonopaths
    putStrLn $ "converted curves: " ++ show (length curves)
    writeFile "log/converted.txt" $ unlines $ map show curves
    since start

    {-
    -- get collection of line segments
    let segs = map (bimap (const ()) id) $ map _core $ map seg curves
    putStrLn $ "segments: " ++ show (length segs)
    writeFile "log/segments.txt" $ unlines $ map show segs
    since start
    -}

    -- construct an arrangement from the chopped curves
    let car = constructPlanar curves
    putStrLn $ "curve arrangement:" 
    writeFile "log/car.txt" $ show car
    since start


    return car


-- read a supposedly planar and already snapped ipe file
fromSnapped :: (RealFrac r, Coordinate r, Show r) => FilePath -> IO (CA r)
fromSnapped filePath = do
  let base   = dropExtensions filePath
      input  = addExtension base ".snapped.ipe"

  -- get the current time
  start <- time
  since start

  -- read the ipe page
  page <- readIpePage input -- :: IO (IpePage r)
  let -- paths :: [Path r :+ IpeAttributes Path r]
      paths = concatMap objectPaths $ view content page
  putStrLn $ "page paths: " ++ show (length paths)
  writeFile "log/oldpaths.txt" $ unlines $ map show paths
  since start


  let nonopaths = separatePaths paths

  -- extract the contents of the page as a set of curves
  let -- curves :: [BezierSpline 3 2 r :+ IpeAttributes Path r]
      curves = convertPathsToBeziers nonopaths
  putStrLn $ "converted curves: " ++ show (length curves)
  writeFile "log/converted.txt" $ unlines $ map show curves
  since start

{-
  -- get collection of line segments
  let segs = map (bimap (const ()) id) $ map _core $ map seg curves
  putStrLn $ "segments: " ++ show (length segs)
  writeFile "log/segments.txt" $ unlines $ map show segs
  since start
-}

  -- construct an arrangement from the chopped curves
  let car = constructPlanar curves
  putStrLn $ "curve arrangement:" 
  writeFile "log/car.txt" $ show car
  since start


  return car






-- | Given two file paths, checks if the second path either does not exist or has an
--   older time stamp than the first.
necessary :: FilePath -> FilePath -> IO Bool
necessary path1 path2 = do
  e1 <- doesFileExist path1
  e2 <- doesFileExist path2
  when (not e1) $ error $ path1 ++ " does not exist"
  if (not e2) then return True else do
    t1 <- getModificationTime path1
    t2 <- getModificationTime path2
    return (t1 > t2)
