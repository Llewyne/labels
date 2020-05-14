module Drawing where

import Graphics.Gloss


width = 600

height = 600

boxSize = 1

boxPath = [(0,0),(boxSize,0),(boxSize,boxSize),(0,boxSize)]

hw = width / 2

hh = height / 2

main 
	= display
         (InWindow
	       "Labels" 	 -- window title
		(floor width + 300, floor height + 300) 	 -- window size
		(100, 100)) 	 -- window position
	white			 -- background color
	picture			 -- picture to display

picture
    = Translate (-hw) (-hh) $ Pictures ((grid width height): (nonogram [] [] 100 200  [1,0.4,0.5] [10,20,30] ))
    	--PicturePictures (concat [(leader (100,1) [1] 3 (-45) 1),(leader (100,3) [1] 2 0 1), (leader (100,5) [1] 4 (45) 1)])

    -- array ((1,1),(4,4)) [((1,1),True),((1,2),False),((1,3),False),((1,4),False),((2,1),True),((2,2),False),((2,3),False),((2,4),False),((3,1),True),((3,2),False),((3,3),False),((3,4),False),((4,1),True),((4,2),False),((4,3),False),((4,4),False)]


label numbers port position "bottom" = _label numbers port position (-1*boxSize)
label numbers port position "top" = _label numbers port position 0

_label numbers port position x
	= Line [port,position]: map (Translate (fst position) (snd position)) (zipWith (boxr (angle port position )) [(a,b)|a<-[0,boxSize..fromIntegral(length numbers)*boxSize],b<-[x]] numbers)

-- Rotated box
boxr rotation position i
    = Pictures [Rotate rotation $ lineLoop boxPath,Rotate rotation $ Translate 2 2 $  Scale 0.1 0.1 $ Text (show i)]

--Unrotated box (no clues)
box o position = Pictures [Translate (position*boxSize) ((fromIntegral o)*boxSize*(-1)) $ lineLoop boxPath]

--Rotated clue
clue o m position i = Pictures [Translate (position*boxSize) ((fromIntegral o)*boxSize*(-1)) $ Scale (boxSize/120) (boxSize/120) $ Translate 10 10 $ Text (show i)]

offset position path
	= map (plus position) path

plus (a,b) (c,d) = (a+c,b+d)
minus (a,b) (c,d) = (a-c,b-d)

nc p = plus p (-hw,-hh) 

angle (a,b) (c,d) = -(atan ((d-b)/(c-a))) * (180/pi)

-- Functions required
-- Draw line (leader) from starting point (port) length (extent) angle and numbers
-- The text needs to not rotate
leader :: (Float,Float) -- The position of the port
	-> [Int] 			-- The list of numbers(clues) that need to be shown
	-> Float 			-- The length of the leader
	-> Float 			-- The slope of the leader
	-> Int 				-- The side of the line 1: bottom and 0: top
	-> [Picture]
leader p@(x,y) numbers len m o = map (Translate x y . Scale 20 20 . Color red) ((Rotate ((atan (-m))*180/pi) $ Line [(0,0),(len,0)]): map (Rotate ((atan (-m))*180/pi)) ( map (Translate (len-(isLeft*(fromIntegral(length numbers))*boxSize)) 0) ((map (box o) [0..fromIntegral((length numbers)-1)]) ++ zipWith (clue o (m)) [0..fromIntegral(length numbers)] numbers)))
    where 
        isLeft
            | len > 0 = 0
            | otherwise = 1 
            
nonogram ::   [[Int]]			-- A list of clues (above the line)
	-> [[Int]]			-- A list of clues (below the line)
	-> Float			-- The width of the box
	-> Float	
	-> [Float]		-- A list of slopes
	-> [Float]			-- The height of the box
	-> [Picture]
nonogram cs1 cs2 w h ms bs = (zipWith (lineIn w h) ms bs) ++ [(lineLoop [(0,0),(w,0),(w,h),(0,h)])]

lineIn :: Float -> Float -> Float -> Float -> Picture
lineIn w h m b =  Line [p1,p2]  
	where
		p1
			| b >=0 && b < h = (0,b)
			| b < 0 = (-b/m,0)
			| b > h = ((h-b)/m,h)
		p2
			| m*w+b >= 0 && m*w+b <= h = (w,m*w+b)
			| m*w+b < 0 = (-b/m,0)
			| m*w+b > h = ((h-b)/m,h)

lineSlopePoint :: Float -> Float->Float -> (Float,Float)->Picture
lineSlopePoint w h m (x,y)= lineIn w h m (y-(m*x))

-- Draws a grid with 100 by 100 squares
grid :: Float -> Float -> Picture
grid w h = Pictures [Color (greyN 0.8) (Pictures $ (map (\x->Pictures[Line [(0,x),(w,x)],Translate (-100) (x-5) $ Scale 0.2 0.2 $  Text (show (round x))]) [0,100..h] ++ map (\x->Pictures[Line [(x,0),(x,h)],Translate (x-5) (-50) $ Scale 0.2 0.2 $  Text (show (round x))]) [0,100..w])),
	Color (greyN 0.9) (Pictures $ (map (\x->Pictures[Line [(0,x),(w,x)],Translate (-50) (x-5) $ Scale 0.1 0.1 $  Text (show (round x))]) [50,150..h-50] ++ map (\x->Pictures[Line [(x,0),(x,h)],Translate (x-5) (-25) $ Scale 0.1 0.1 $  Text (show (round x))]) [50,150..w-50]))]

getPointAtX :: Float -> Float -> Float -> (Float,Float)
getPointAtX x m b = (x,m*x+b)

getPointAtY :: Float -> Float -> Float -> (Float,Float)
getPointAtY y m b = ((y-b)/m,y)

getPort1 :: Float -> Float -> Float -> Float -> (Float,Float)
getPort1 w h m b 
    | b >=0 && b < h = (0,b)
    | b < 0 = (-b/m,0)
    | b > h = ((h-b)/m,h)

getPort2 :: Float -> Float -> Float -> Float -> (Float,Float)
getPort2 w h m b 
    | m*w+b >= 0 && m*w+b <= h = (w,m*w+b)
    | m*w+b < 0 = (-b/m,0)
    | m*w+b > h = ((h-b)/m,h)

getB :: Float -> (Float,Float) ->Float
getB m (x,y) = y-m*x

nonoline :: Float -> Float -> Float -> Float -> [Int] -> [Int] -> Picture
nonoline w h m b tops bottoms = Pictures (lineIn w h m b : leader (getPort1 w h m b) tops (-1) m 0 ++ leader (getPort2 w h m b) bottoms 1 m 1)