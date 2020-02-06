module Drawing where

import Graphics.Gloss


width = 1000

height = 1000

boxSize = 15

boxPath = [(0,0),(boxSize,0),(boxSize,boxSize),(0,boxSize)]

hw = width / 2

hh = height / 2

main 
	= display
         (InWindow
	       "Labels" 	 -- window title
		(floor width, floor height) 	 -- window size
		(100, 100)) 	 -- window position
	white			 -- background color
	picture			 -- picture to display

picture
    = Pictures (concat [(leader (100,100) [1,2,3] 100 45 1),(leader (100,300) [1] 100 0 0), (leader (100,500) [1] 100 (-45) 1)])

    -- array ((1,1),(4,4)) [((1,1),True),((1,2),False),((1,3),False),((1,4),False),((2,1),True),((2,2),False),((2,3),False),((2,4),False),((3,1),True),((3,2),False),((3,3),False),((3,4),False),((4,1),True),((4,2),False),((4,3),False),((4,4),False)]


label numbers port position "bottom" = _label numbers port position (-1*boxSize)
label numbers port position "top" = _label numbers port position 0

_label numbers port position x
	= Line [port,position]: map (Translate (fst position) (snd position)) (zipWith (boxr (angle port position )) [(a,b)|a<-[0,boxSize..fromIntegral(length numbers)*boxSize],b<-[x]] numbers)

-- Rotated box
boxr rotation position i
    = Pictures [Rotate rotation $ lineLoop boxPath,Rotate rotation $ Translate 2 2 $  Scale 0.1 0.1 $ Text (show i)]

--Unrotated box
box o position i = Pictures [Translate position ((fromIntegral o)*boxSize*(-1)) $ lineLoop boxPath,Translate 2 (2+((fromIntegral o)*boxSize*(-1))) $  Scale 0.1 0.1 $ Text (show i)]

offset position path
	= map (plus position) path

plus (a,b) (c,d) = (a+c,b+d)
minus (a,b) (c,d) = (a-c,b-d)

nc p = plus p (-hw,-hh) 

angle (a,b) (c,d) = -(atan ((d-b)/(c-a))) * (180/pi)

-- Functions required
-- Draw line (leader) from starting point (port) length (extent) angle and numbers
leader :: (Float,Float) -> [Int] -> Float -> Float -> Int -> [Picture]
leader p@(x,y) numbers len m o = map (Translate x y) ((Rotate m $ Line [(0,0),(len,0)]): map (Rotate m) ( map (Translate len 0) (zipWith (box o) [0..fromIntegral(length numbers)*boxSize] numbers)))

