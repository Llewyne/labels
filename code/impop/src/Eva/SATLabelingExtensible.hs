{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Eva.SATLabelingExtensible where

import SAT.Mios (CNFDescription (..), solveSAT)
import Control.Lens
import Nonogram
import Data.Foldable (toList)

import Data.Geometry hiding (head,direction,init)
import Data.Geometry.Polygon hiding (minimumBy)
import Data.Geometry.Boundary
import Data.Ext
import Data.List hiding (intersect)
import Data.Function (on)
import System.Process

import Debug.Trace
import Eva.ClueBox

type UnplacedLabelExt = ([(Port,Float)], Clue)

type Clause = ([Int], Float)

maxWeight = 10000
eMax = 5

placeLabelsSATExtensible :: [UnplacedLabel] -> IO [Label]
placeLabelsSATExtensible ul = do
    let ml = mapVars (map makeUnplacedLabelExt ul)
    -- mapM_ (putStrLn . show) ml
    let clauses = setClauses ml
    -- putStrLn $ show clauses
    -- let desc = CNFDescription (length $ concat ml) (length clauses) ""
    printClauses ul
    asg <- runMaxHS
    -- putStrLn $ show asg
    let allLabels = map (makeLabelExt (concat ml)) $ filter (0<) asg
    let bestLabels = map (minimumBy (compare `on` _offset)) (groupBy (\x y -> _port x == _port y) allLabels)
    return bestLabels

printClauses :: [UnplacedLabel] -> IO ()
printClauses ul = do
    let ml = mapVars (map makeUnplacedLabelExt ul)
    let clauses = setClauses ml
    let allClause = (map makeHardClause clauses) ++ map setSoftClauses (concat ml)
    let wClauses = map writeClause allClause
    let t = ("c\nc test max format\nc\np wcnf " ++ show (length $ concat ml) ++ " " ++ show (length wClauses) ++ "\n") ++ concat wClauses
    writeFile "testformat.txt" t

runMaxHS :: IO [Int]
runMaxHS = do
    output <- readProcess "wsl" ["../../MaxHS-3.2/build/release/bin/maxhs","testformat.txt"] ""
    let sol = map read $ tail $ words $ head $ filter (\x -> head x == 'v') (lines output)-- (head $ filter (\x -> head x == "v") (lines output))
    return $ filter (0 <) sol

writeClause ::  Clause -> String
writeClause (c,w) = (showDecimal $ realToFrac w) ++ " " ++ (concat $ map (\x -> show x ++ " ") c) ++ "0\n"

showDecimal :: Double -> String
showDecimal d = show (floor d) ++ "." ++ show (floor((d-(fromIntegral(floor d)))*100))

makeHardClause :: [Int] -> Clause
makeHardClause c = (c,maxWeight)

makeLabel :: [(Int,(UnplacedLabel,Int))] -> Int -> Label
makeLabel ml i = Label clue port 0
        where
            clue = snd $ fst $ snd $ ml!!(i-1)
            port = fst ( fst $ snd $ ml!!(i-1))!!(snd ( snd $ ml!!(i-1))-1)

makeLabelExt :: [(Int,(UnplacedLabelExt,Int))] -> Int -> Label
makeLabelExt ml i = Label clue port len
        where
            clue = snd $ fst $ snd $ ml!!(i-1)
            port = fst (fst ( fst $ snd $ ml!!(i-1))!!(snd ( snd $ ml!!(i-1))-1))
            len = snd (fst ( fst $ snd $ ml!!(i-1))!!(snd ( snd $ ml!!(i-1))-1))

makeUnplacedLabelExt :: UnplacedLabel -> UnplacedLabelExt
makeUnplacedLabelExt ul = ([(p,ls)|p<-fst ul,ls<-[1..eMax]],snd ul)


-- Sets the clauses
-- for each line l with ports p and q, xal = 1 (xal = 0) if the label above l is assigned to p (q). 
-- And xbl = 1 (xbl = 0) if the label below l is assigned to p (q)
-- If two possible labels overlap then, for instance the label above l when assigned to port p and the label below l' when assigned to port p'
-- then  the clause: -xal or -xb'l is added
-- also for each line l the clauses: xal or xbl, -xal or -xbl to ensure each line has exactly one label above and below
setClauses :: [[(Int,(UnplacedLabelExt,Int))]] -> [[Int]]
setClauses ml = map (\((a,_),(b,_))->[-a,-b]) (filter overlap (filter differentPort (pairs (concat ml)))) -- clauses for overlapping 
            ++ concatMap (\ls->[[-(fst $ head ls),-(fst $ ls!!1),-(fst $ ls!!2),-(fst $ ls!!3)],[fst $ head ls,fst $ ls!!1,fst $ ls!!2,fst $ ls!!3]]) ( filter (\x->length x == 2) ml) -- make sure 1 of 2 ports is selected for every line
            ++ map (\ls->[fst $ head ls]) (filter (\x->length x == 1) ml)

setSoftClauses :: (Int,(UnplacedLabelExt,Int)) -> Clause
setSoftClauses (c,(ul,p)) = ([c],- (log ((snd ((fst ul)!!(p-1)))/eMax)))


differentPort :: ((Int,(UnplacedLabelExt,Int)),(Int,(UnplacedLabelExt,Int)))-> Bool
differentPort ((_,l1),(_,l2)) = _location (getPort l1) /= _location (getPort l2)

getPort :: (UnplacedLabelExt,Int) -> Port
getPort ((ps,_),i) = fst (ps!!(i-1))

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

-- map the variables for the SAT clauses, result is a list of pairs where the first element is the index of the variable and the second element the port assignment
mapVars :: [UnplacedLabelExt] -> [[(Int,(UnplacedLabelExt,Int))]]
mapVars ul = _mapVars (map getVar ul) 1

_mapVars :: [[(UnplacedLabelExt,Int)]] -> Int -> [[(Int,(UnplacedLabelExt,Int))]]
_mapVars [] _ = []
_mapVars (v:vs) i = zip [i..] v : _mapVars vs (i + (length v))

getVar :: UnplacedLabelExt -> [(UnplacedLabelExt,Int)]
getVar ul = [(a,b)|a<-[ul],b<-[1..(length (fst ul))] ]

-- Determines if two labels overlap
overlap :: ((Int,(UnplacedLabelExt,Int)),(Int,(UnplacedLabelExt,Int)))-> Bool
overlap ((_,(l1,i1)),(_,(l2,i2))) = intersects (clueBox (fst((fst l1)!!(i1-1)))) (clueBox (fst((fst l2)!!(i2-1))))


size :: Rational
size = 16


-- Gives the cluebox as a polygon from the port
-- Need to convert everything to rational since the intersection functions don't always work otherwise
clueBox :: Port -> ClueBox
clueBox (Port p v s) = clueBoxPolygon p v s 1

-- clueBox (Port (Point2 lx ly) (Vector2 vx vy) False) = fromPoints $ map ext ([Point2 _lx _ly,Point2 (_lx-(_vy*size)) (_ly+(_vx*size)), Point2 (_lx+(_vx*size)) (_ly+(_vy*size)),Point2 (_lx+((_vx*size)-(_vy*size))) (_ly+((_vy*size)+(_vx*size)))] :: [Point 2 Rational])
--    where
--         _lx = toRational lx
--         _ly = toRational ly
--         _vx = toRational vx
--         _vy = toRational vy

