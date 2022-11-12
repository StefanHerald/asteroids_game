module Objects where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
--basic types
type Dir      = (Float, Float)
type Pos      = (Float, Float)
type Health   = Int

--typeclass for displaying on the screen
class OnScreen a where
    onScreen :: a -> Picture
data Obstacle = Enemy Health Pos Dir
                | Asteroid Health Pos Dir
                | Mine Health Pos
                | Projectile Pos Dir
                deriving (Show)

instance OnScreen Obstacle where
    onScreen (Enemy _ pos dir)    = color cyan   (rotateAndTranslate pos dir   (text "A"))
    onScreen (Asteroid _ pos dir) = color brown  (rotateAndTranslate pos dir   (text "O"))
    onScreen (Mine _ pos)         = color red    (rotateAndTranslate pos (0,0) (text "X"))
    onScreen (Projectile pos dir) = color yellow (rotateAndTranslate pos dir   (text "I"))

rotateAndTranslate :: Pos -> Dir -> Picture -> Picture
rotateAndTranslate (px, py) dir pic = translate px py (rotate (argV dir) pic)

brown :: Color
brown = makeColorI 255 248 220 255

data Player = Player Health Pos Dir
              deriving (Show)

instance OnScreen Player where
    onScreen (Player _ pos dir) = color green (rotateAndTranslate pos dir (text "Y"))

initialPlayer :: Player
initialPlayer = Player 3 (300, 300) (0,0)