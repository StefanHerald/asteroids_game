module Collision where
import GameState
import Objects
import Data.List
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

checkPlayerCollision :: GameState -> Obstacle -> GameState
checkPlayerCollision g@(GameState os p@(Player h (x, y) (dx, dy) ps) s t d _) o@(Enemy oh op@(ex, ey) odir@(odx, ody))
    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) < (ex + 10)) && ((y - 10) < (ey + 10)) = GameState ((Enemy (oh - 1) (ex - odx, ey - ody) (-odx, -ody)) : (delete o os)) (Player (h - 1) (x - (1 * dx), y - (1 * dy)) (-1 * dx, -1 * dy) ps) s t d False
    | otherwise = g
checkPlayerCollision g@(GameState os p@(Player h (x, y) (dx, dy) ps) s t d _) o@(Asteroid oh op@(ex, ey) odir@(odx, ody))
    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) < (ex + 10)) && ((y - 10) < (ey + 10)) = GameState ((Asteroid (oh - 1) (ex - odx, ey - ody) (-odx, -ody)) : (delete o os)) (Player (h - 1) (x - (1 * dx), y - (1 * dy)) (-1 * dx, -1 * dy) ps) s t d False
    | otherwise = g
checkPlayerCollision g@(GameState os p@(Player h (x, y) (dx, dy) ps) s t d _) o@(Mine oh op@(ex, ey))
    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) < (ex + 10)) && ((y - 10) < (ey + 10)) = GameState (delete o os) (Player (h - 3) (x - (1 * dx), y - (1 * dy)) (-1 * dx, -1 * dy) ps) s t d False
    | otherwise = g
checkPlayerCollision g _ = g

checkPlayerProjectileCollision :: Obstacle -> GameState -> Obstacle -> GameState
checkPlayerProjectileCollision pr@(Projectile (x, y) dir) g@(GameState os p@(Player h pos pdir ps) s t d _) o@(Enemy oh op@(ex, ey) odir)
    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) < (ex + 10)) && ((y - 10) < (ey + 10)) = GameState ((Enemy (oh - 1) op odir) : (delete o os)) (Player h pos pdir (filter (\x -> x /= pr) ps)) s t d False
    | otherwise = g
checkPlayerProjectileCollision pr@(Projectile (x, y) dir) g@(GameState os p@(Player h pos pdir ps) s t d _) o@(Asteroid oh op@(ex, ey) odir)
    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) < (ex + 10)) && ((y - 10) < (ey + 10)) = GameState ((Asteroid (oh - 1) op odir) : (delete o os)) (Player h pos pdir (filter (\x -> x /= pr) ps)) s t d False
    | otherwise = g
checkPlayerProjectileCollision pr@(Projectile (x, y) dir) g@(GameState os p@(Player h pos pdir ps) s t d _) o@(Mine oh op@(ex, ey))
    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) < (ex + 10)) && ((y - 10) < (ey + 10)) = GameState ((Mine (oh - 1) op) : (delete o os)) (Player h pos pdir (filter (\x -> x /= pr) ps)) s t d False
    | otherwise = g
checkPlayerProjectileCollision _ g _ = g