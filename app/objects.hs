module Objects where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
--basic types
type Dir      = (Float, Float)
type Pos      = (Float, Float)
type Health   = Int

--typeclass for displaying on the screen
class ToFile a where
    toFile :: a -> String
class OnScreen a where
    onScreen :: a -> Picture
data Obstacle = Enemy Health Pos Dir
                | Asteroid Health Pos Dir
                | Mine Health Pos
                | Projectile Pos Dir
                | Animation Pos Int Float
                deriving (Show, Eq)

newAnimation :: Pos -> Obstacle 
newAnimation pos = Animation pos 0 0
instance OnScreen Obstacle where
    onScreen (Enemy _ pos dir)    = color cyan   (resizeRotateAndTranslate (0.2, 0.2) pos dir   (text "A"))
    onScreen (Asteroid _ pos dir) = color brown  (resizeRotateAndTranslate (0.2, 0.2) pos dir   (text "O"))
    onScreen (Mine _ pos)         = color red    (resizeRotateAndTranslate (0.2, 0.2) pos (0,0) (text "X"))
    onScreen (Projectile pos dir) = color yellow (resizeRotateAndTranslate (0.2, 0.2) pos dir   (text "I"))
    onScreen (Animation pos 0 _)    = color yellow (resizeRotateAndTranslate (0.2, 0.2) pos (0,0) (text "X"))
    onScreen (Animation pos 1 _)    = color orange  (resizeRotateAndTranslate (0.25, 0.25) pos (0,0) (text "O"))
    onScreen (Animation pos _ _)    = color red  (resizeRotateAndTranslate (0.3, 0.3) pos (0,0) (text "O"))

instance ToFile Obstacle where
    toFile (Enemy health (px, py) (dx, dy)) = "E " ++ 
                                            show health ++ " " ++ 
                                            show px ++ " " ++ 
                                            show py ++ " " ++ 
                                            show dx ++ " " ++ 
                                            show dy ++ " " 
    toFile (Asteroid health (px, py) (dx, dy)) = "A " ++ 
                                                show health ++ " " ++ 
                                                show px ++ " " ++ 
                                                show py ++ " " ++ 
                                                show dx ++ " " ++ 
                                                show dy ++ " "
    toFile (Mine health (px, py)) = "M " ++ 
                                    show health ++ " " ++ 
                                    show px ++ " " ++ 
                                    show py ++ " "
    toFile (Projectile (px, py) (dx, dy)) = "P" ++ " " ++ 
                                            show px ++ " " ++ 
                                            show py ++ " " ++ 
                                            show dx ++ " " ++ 
                                            show dy ++ " "  
    toFile (Animation _ _ _)   = ""

resizeRotateAndTranslate :: (Float, Float) -> Pos -> Dir -> Picture -> Picture
resizeRotateAndTranslate (sx, sy) (px, py) dir pic = translate px py (rotate (argV dir) (scale sx sy pic))

brown :: Color
brown = makeColorI 255 248 220 255

data Player = Player{ health :: Health, position :: Pos, direction :: Dir, projectiles :: [Obstacle]}
              deriving (Show)

instance OnScreen Player where
    onScreen (Player _ pos dir pro) = pictures ((color green (resizeRotateAndTranslate (0.2, 0.2 ) pos dir (text "Y"))) : (map onScreen pro))

instance ToFile Player where
    toFile (Player health (px, py) (dx, dy) pro) = "W" ++ " " ++ 
                                                    (show health) ++ " " ++
                                                    (show px) ++ " " ++ 
                                                    (show py) ++ " " ++ 
                                                    (show dx) ++ " " ++ 
                                                    (show dy) ++ " " ++ 
                                                    (concat (map toFile pro))
initialPlayer :: Player
initialPlayer = Player 10 (0, 0) (0, 0) []