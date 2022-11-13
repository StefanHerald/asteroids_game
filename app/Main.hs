module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import GameState
import Objects
import Control.Monad

main :: IO ()
main = playIO (InWindow "Asteroids" (600, 600) (0, 0)) --window name: asteroids, 600x600
               black                                   --background
               20                                      --framerate
               initialGameState                        --the first gamestate 
               view                                    --how to draw everything
               input                                   --how to handle input from the keyboard
               step                                    -- advance the rest of the world     

view :: GameState -> IO Picture --impure function returning the view
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture --pure function for the view
viewPure (GameState obs pl sc time dif) = pictures [drawSc, drawT, drawDif, drawObs, drawPl]
    where
        drawObs = pictures (map onScreen obs)
        drawPl  = onScreen pl
        drawSc  = resizeRotateAndTranslate (0.15, 0.15) (0, 283) (0,0) (color orange (text (show sc)))
        drawT   = resizeRotateAndTranslate (0.15, 0.15) (0, 260) (0,0) (color white (text (show (round time))))
        drawDif = resizeRotateAndTranslate (0.15, 0.15) (-280, 280) (0,0) (color red (text (show dif)))

input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate) 

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d) = GameState os (Player h pos (dx, dy - 5) ps) s t d
handleInput (EventKey (Char 'a') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d) = GameState os (Player h pos (dx - 5, dy) ps) s t d
handleInput (EventKey (Char 's') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d) = GameState os (Player h pos (dx, dy + 5) ps) s t d
handleInput (EventKey (Char 'd') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d) = GameState os (Player h pos (dx + 5, dy) ps) s t d
handleInput _                          (GameState os (Player h pos (dx, dy) ps) s t d) = GameState os (Player h pos (0.75 * dx, 0.75 * dy) ps) s t d

step :: Float -> GameState -> IO GameState --secs is truly the amount of secs
step secs gstate = return ( gstate{timer = timer gstate + secs })

updateGamestate :: GameState -> GameState
updateGamestate (GameState os p s t d) = GameState (map moveObjects os) (movePlayer p) s t d

moveObjects :: Obstacle -> Obstacle
moveObjects (Enemy h (x, y) dir@(dx, dy)) = Enemy h (x + dx, y + dy) dir
moveObjects (Asteroid h (x, y) dir@(dx, dy)) = Asteroid h (x + dx, y + dy) dir
moveObjects (Projectile (x, y) dir@(dx, dy)) = Projectile (x + dx, y + dy) dir
moveObjects a = a

movePlayer :: Player -> Player
movePlayer (Player h (x, y) dir@(dx, dy) ps) = Player h (x + dx, y + dy) dir ps

enemyAI :: Obstacle -> Player -> Obstacle
enemyAI e@(Enemy h (x, y) dir) (Player _ (px, py) _ _) = (movetoIdealState e (Enemy h idealPos idealDir))
   where
      idealPos = (posWithMinDistance (map (distance (x, y)) [(px + 20, py), (px - 20, py), (px, py + 20), (px, py - 20)]))   --Can replace with formula for circle around player with radius 20
      idealDir = ((px - x) / (getDistanceNoPos (distance (x, y) (px, py))), (py - y) / (getDistanceNoPos (distance (x, y) (px, py))))

getDistanceNoPos :: (Pos, Float) -> Float
getDistanceNoPos (a, b) = b

posWithMinDistance :: [(Pos, Float)] -> Pos
posWithMinDistance [a, b, c, d] = (partOfAnInefficientFunction (posWithMinDistance_ a b) (posWithMinDistance_ c d))

posWithMinDistance_ :: (Pos, Float) -> (Pos, Float) -> (Pos, Float)
posWithMinDistance_ (a, b) (c, d)
                                 | b > d = (a, b)
                                 | otherwise = (c, d)

partOfAnInefficientFunction :: (Pos, Float) -> (Pos, Float) -> Pos
partOfAnInefficientFunction (a, b) (c, d)
                                         | b > d = a
                                         | otherwise = c

distance :: Pos -> Pos -> (Pos, Float)
distance (x1, y1) (x2, y2) = ((x2, y2), (sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))))

movetoIdealState :: Obstacle -> Obstacle -> Obstacle
movetoIdealState (Enemy h pos@(x, y) (dx, dy)) (Enemy _ (ix, iy) iDir@(idx, idy))
                                                                                 | x > (ix - 5) && x < (ix + 5) && y > (iy - 5) && y < (iy + 5) = Enemy h pos iDir
                                                                                 | otherwise = Enemy h pos ((max (abs (ix - x)) 5), (max (abs (iy - y)) 5))