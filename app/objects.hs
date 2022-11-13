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
    onScreen (Enemy _ pos dir)    = color cyan   (resizeRotateAndTranslate (0.2, 0.2) pos dir   (text "A"))
    onScreen (Asteroid _ pos dir) = color brown  (resizeRotateAndTranslate (0.2, 0.2) pos dir   (text "O"))
    onScreen (Mine _ pos)         = color red    (resizeRotateAndTranslate (0.2, 0.2) pos (0,0) (text "X"))
    onScreen (Projectile pos dir) = color yellow (resizeRotateAndTranslate (0.2, 0.2) pos dir   (text "I"))

resizeRotateAndTranslate :: (Float, Float) -> Pos -> Dir -> Picture -> Picture
resizeRotateAndTranslate (sx, sy) (px, py) dir pic = translate px py (rotate (argV dir) (scale sx sy pic))

brown :: Color
brown = makeColorI 255 248 220 255

data Player = Player{ health :: Health, position :: Pos, direction :: Dir, projectiles :: [Obstacle]}
              deriving (Show)

instance OnScreen Player where
    onScreen (Player _ pos dir pro) = pictures ((color green (resizeRotateAndTranslate (0.2, 0.2 ) pos dir (text "Y"))) : (map onScreen pro))

initialPlayer :: Player
initialPlayer = Player 3 (0, 0) (0, 0) []