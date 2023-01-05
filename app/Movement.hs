module Movement where
import GameState
import Objects
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

movePlayer :: Player -> Player
movePlayer (Player h (x, y) (dx, dy) ps) = Player h (x + dx, y + dy) (0.94 * dx, 0.94 * dy) ps

moveObjects :: Player -> Obstacle -> Obstacle
moveObjects p (Enemy h (x, y) dir@(dx, dy)) = enemyAI (Enemy h (x + dx, y + dy) dir) p
moveObjects _ (Asteroid h (x, y) dir@(dx, dy)) = Asteroid h (x + dx, y + dy) dir
moveObjects _ (Projectile (x, y) dir@(dx, dy)) = Projectile (x + dx, y + dy) dir
moveObjects _ a = a


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
    | otherwise = Enemy h pos ((ix - x) / (getDistanceNoPos (distance (x, y) (ix, iy))), (iy - y) / (getDistanceNoPos (distance (x, y) (ix, iy))))
