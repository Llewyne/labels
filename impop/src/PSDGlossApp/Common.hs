{-# LANGUAGE TemplateHaskell #-}

module PSDGlossApp.Common where

import Control.Lens

import Graphics.Gloss hiding (Point, Vector, Arc, arc, display)
import Graphics.Gloss.Interface.Pure.Game hiding (Point, Vector, Arc, arc)


import Data.Geometry.PlanarSubdivision



standardDisplay :: Display
standardDisplay = InWindow "Window" (800, 800) (100, 100)

standardBackground :: Color
standardBackground = white

standardRate :: Int
standardRate = 20


data PSE s = V (VertexId' s) | D (Dart s) | F (FaceId' s) | N deriving (Eq, Ord, Show)

data State s v e f r = State
  { _subdivision  :: PlanarSubdivision s v e f r 
  , _hover        :: PSE s
  , _hoverChanged :: Bool
  , _selection    :: [PSE s]
  } deriving (Show, Eq)
$(makeLenses ''State)
-- lenses here?

data App s v e f r = App
  { _drawState   :: State s v e f r -> Picture
  , _handleEvent :: Event -> State s v e f r -> State s v e f r
  , _passTime    :: Float -> State s v e f r -> State s v e f r
  }
$(makeLenses ''App)


app :: App s v e f r
app = App (const Blank) (const id) (const id)
  
run :: (App s v e f r -> App s v e f r) -> PlanarSubdivision s v e f r -> IO ()
run f psd = play
  standardDisplay
  standardBackground
  standardRate
  (State psd N True [])
  (f app ^. drawState)
  (f app ^. handleEvent)
  (f app ^. passTime)




type Behaviour s v e f r = App s v e f r -> App s v e f r
-- maybe reconsider changing the extra app data?


-- is there an operator of type ?
(%~~) :: ASetter' s (a -> b -> b) -> (a -> b -> b) -> s -> s
l %~~ f = l %~ (\g s -> g s . f s)
-- define infixl
infixr 4 %~~