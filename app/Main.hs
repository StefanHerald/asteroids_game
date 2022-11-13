module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import GameState
import Objects
import Control.Monad
import System.Random
import Data.List
import Data.Char (toUpper)

main :: IO ()
main = playIO (InWindow "Asteroids" (600, 600) (0, 0)) --window name: asteroids, 600x600
               black                                   --background
               20                                      --framerate
               initialGameState                        --the first gamestate 
               view                                    --how to draw everything
               input                                   --how to handle input from the keyboard
               step                                    -- advance the rest of the world     

view :: GameState -> IO Picture --impure function returning the view
view gstate@(GameState obs pl@(Player 0 _ _ _) score _ _ _) = do
    x <- readFile "../asteroids_game/saves/highscore.txt"
    return $ pictures $ viewPure gstate : whatToPress : placeHigh x : addOnDeath 
    where
         addOnDeath = [resizeRotateAndTranslate (0.6, 0.6) (-280, 200) (0,0) (color white (text "You have lost!")),
                    resizeRotateAndTranslate (0.2, 0.2) (-280, 120) (0,0) (color orange (text $ "This time: " ++ show score))]
         placeHigh sc = resizeRotateAndTranslate (0.2, 0.2) (-280, 60) (0,0) (color green (text $ "Best of all time: " ++ sc ))
         whatToPress =  resizeRotateAndTranslate (0.2, 0.2) (-280, 0) (0,0) (color white (text "Press k to restart or r to reload") )
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture --pure function for the view
viewPure (GameState obs pl@(Player h _ _ _) sc time dif b) 
    | h /= 0 && b  = pictures ((dimAll full) ++ addToPaused)
    | otherwise    = pictures full
    where
        dimAll :: [Picture] -> [Picture]
        dimAll []              = []
        dimAll((Color c p):xs) = (Color (dim c) p) : dimAll xs
        dimAll (x:xs)          = x : dimAll xs
        addToPaused = [resizeRotateAndTranslate (0.7, 0.7) (-280, 200) (0,0) (color white (text "| |")),
                       resizeRotateAndTranslate (0.2, 0.2) (-280, 120) (0,0) (color white (text "Press space to unpause")),
                       resizeRotateAndTranslate (0.2, 0.2) (-280, 40) (0,0) (color white (text "Press r to save the current state")),
                       resizeRotateAndTranslate (0.2, 0.2) (-280, -80) (0,0) (color white (text "Press l to load the previous save"))]
        full = [drawSc, drawT, drawDif, drawObs, drawPl]
        drawObs  = pictures (map onScreen obs)
        drawPl   = onScreen pl
        drawSc   = resizeRotateAndTranslate (0.15, 0.15) (0, 283) (0,0) (color orange (text (show sc)))
        drawT    = resizeRotateAndTranslate (0.15, 0.15) (0, 260) (0,0) (color white (text (show (round time))))
        drawDif  = resizeRotateAndTranslate (0.15, 0.15) (-280, 280) (0,0) (color red (text (show dif)))

input :: Event -> GameState -> IO GameState
input e gstate@(GameState _ (Player 0 _ _ _) score _ _ False) = do 
    writeScore score
    return gstate{paused = True}
input e gstate@(GameState _ (Player 0 _ _ _) _ _ _ True) = handleDeath e gstate
input e gstate | paused gstate = handlePause e gstate
               | otherwise = return (handleInput e gstate) 

writeScore :: Int -> IO()
writeScore score = do
    x <- readFile "../asteroids_game/saves/highscore.txt"
    if score > read x then
        writeFile "../asteroids_game/saves/highscore.txt" (show score)
    else
        return ()

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (dx, 5) ps) s t d False
handleInput (EventKey (Char 'a') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (-5, dy) ps) s t d False
handleInput (EventKey (Char 's') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (dx, -5) ps) s t d False
handleInput (EventKey (Char 'd') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (5, dy) ps) s t d False
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate{paused = True}
handleInput (EventKey (MouseButton LeftButton) _ _ _ ) (GameState os p s t d False) = GameState os (playerShot p) s t d False
handleInput _                          gamestate = gamestate

handleDeath :: Event -> GameState -> IO GameState
handleDeath (EventKey (Char 'k') _ _ _) gstate = return initialGameState
handleDeath (EventKey (Char 'r') _ _ _) gstate = loadGame
handleDeath _ gstate = return gstate 

playerShot :: Player -> Player
playerShot p@(Player h pos dir ps)
                                | length ps < 5 = Player h pos dir (ps ++ [Projectile pos dir])
                                | otherwise = p

step :: Float -> GameState -> IO GameState --secs is truly the amount of secs
step secs gstate@(GameState os p s t d pause) 
    | pause = return gstate
    | otherwise =  do
    g <- getStdGen
    let x = fst (randomR (1, d) g)
    setStdGen (takeSecond (randomR (1, d) g))
    g <- getStdGen
    let y = fst (randomR (1, d) g)
    setStdGen (takeSecond (randomR (1, d) g))
    g <- getStdGen
    let z = fst (randomR (1, 5 * d) g)
    setStdGen (takeSecond (randomR (1, d) g))
    return ( spawnEnemyOrNot (mod x 10) y z (stepObj (updateGamestate gstate{timer = timer gstate + secs })))
                                    
takeSecond :: (a, b) -> b
takeSecond (a, b) = b

spawnEnemyOrNot :: Int -> Int -> Int -> GameState -> GameState
spawnEnemyOrNot x y z g@(GameState os p s t d b)
                                        | z < (max (div d 4) 2) && (length os < 10) = GameState (os ++ [Enemy 3 (fromIntegral (50 * (mod x 12) - 300), fromIntegral (50 * (mod y 12) - 300)) (0, 0)]) p s t d b
                                        | otherwise = GameState os p s t d b

stepObj :: GameState -> GameState --can be called for more than just removing objects with 0 HP
stepObj (GameState obj p sc t d boo) = GameState (removeZeroHp obj) (p{health = max 0 (health p)}) sc t d boo 

removeZeroHp :: [Obstacle] -> [Obstacle]
removeZeroHp [] = []
removeZeroHp (Animation pos stage time : xs) | stage > 2 = removeZeroHp xs
                                             | otherwise = (Animation pos (floor time) (time + 0.05)) : removeZeroHp xs
removeZeroHp (Enemy 0 pos _ : xs )   = newAnimation pos : removeZeroHp xs
removeZeroHp (Asteroid 0 pos _ : xs) = newAnimation pos : removeZeroHp xs
removeZeroHp (Mine 0 pos : xs )      = newAnimation pos : removeZeroHp xs
removeZeroHp (x : xs)                = x : removeZeroHp xs

--pausing, saving and loading the game
handlePause :: Event -> GameState -> IO GameState
handlePause (EventKey key Down _ _) gstate = do
    case key of
        (Char 'a') -> return initialGameState
        (SpecialKey KeySpace ) -> return gstate{paused = False}
        (Char 'r') -> do 
            saveGame gstate
            return gstate
        (Char 'l') -> do
            loadGame
        _  -> return gstate
handlePause _ gstate = return gstate
saveGame :: GameState -> IO()
saveGame gstate = do 
    writeFile "../asteroids_game/saves/save" (toFile gstate)

loadGame :: IO GameState
loadGame = do
    file <- readFile "../asteroids_game/saves/save"
    if file == "" then
        return initialGameState
    else
        let ls = lines file
            obs = loadObs $ words (head ls)
            lss = drop 1 ls
            p = loadPl $ words (head lss)
            lsss = drop 1 lss
            sc = read $ head lsss :: Int
            lssss = drop 1 lsss
            time = read $ head lssss :: Float
            lsssss = drop 1 lssss
            dif = read $ head lsssss :: Int
            in
            return (GameState obs p sc time dif False)

loadObs :: [String] -> [Obstacle]
loadObs [] = []
loadObs ("E":h:px:py:dx:dy:rest) = Enemy (readHealth h) (readPos px py) (readDir dx dy) : loadObs rest
loadObs ("A":h:px:py:dx:dy:rest) = Asteroid (readHealth h) (readPos px py) (readDir dx dy) : loadObs rest
loadObs ("M":h:px:py:rest) = Mine (readHealth h) (readPos px py): loadObs rest
loadObs ("P":px:py:dx:dy:rest) = Projectile (readPos px py) (readDir dx dy)  : loadObs rest
loadObs (x:xs) = []

loadPl :: [String] -> Player
loadPl (_:h:px:py:dx:dy: pro) = Player (readHealth h) (readPos px py) (readDir dx dy) (loadObs pro)
loadPl [] = initialPlayer
loadPl (x:xs) = initialPlayer

readHealth :: String -> Health
readHealth = read  
readPos :: String -> String -> Pos
readPos px py = (read px, read py)
readDir :: String -> String -> Dir
readDir dx dy = (read dx, read dy)

--moving objects and deleting them
updateGamestate :: GameState -> GameState
updateGamestate g@(GameState os p@(Player h pos dir ps) s t d _) = applyChain checkPlayerCollision (GameState (map (moveObjects p) os) (movePlayer (Player h pos dir (map (moveObjects p) (deleteOutOfBounds ps)))) s t d False) os

applyChain :: (c -> b -> c) -> c -> [b] -> c
applyChain f a (b : []) = f a b
applyChain f a (b : bs) = applyChain f (f a b) bs
applyChain _ a [] = a

moveObjects :: Player -> Obstacle -> Obstacle
moveObjects p (Enemy h (x, y) dir@(dx, dy)) = enemyAI (Enemy h (x + dx, y + dy) dir) p
moveObjects _ (Asteroid h (x, y) dir@(dx, dy)) = Asteroid h (x + dx, y + dy) dir
moveObjects _ (Projectile (x, y) dir@(dx, dy)) = Projectile (x + dx, y + dy) dir
moveObjects _ a = a

deleteOutOfBounds :: [Obstacle] -> [Obstacle]
deleteOutOfBounds ps = filter (\(Projectile (x, y) dir) -> x < 300 && y < 300 && x > -300 && y > -300) ps

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

checkPlayerProjectileCollision :: GameState -> Obstacle -> Obstacle -> GameState
checkPlayerProjectileCollision g@(GameState os p@(Player h pos pdir ps) s t d _) pr@(Projectile (x, y) dir) o@(Enemy oh op@(ex, ey) odir)
                                                                    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) > (ex + 10)) && ((y - 10) > (ey + 10)) = GameState ((Enemy (oh - 1) op odir) : (filter (\x -> x /= o) os)) (Player h pos pdir (filter (\x -> x /= pr) ps)) s t d False
                                                                    | otherwise = g
checkPlayerProjectileCollision g@(GameState os p@(Player h pos pdir ps) s t d _) pr@(Projectile (x, y) dir) o@(Asteroid oh op@(ex, ey) odir)
                                                                    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) > (ex + 10)) && ((y - 10) > (ey + 10)) = GameState ((Asteroid (oh - 1) op odir) : (filter (\x -> x /= o) os)) (Player h pos pdir (filter (\x -> x /= pr) ps)) s t d False
                                                                    | otherwise = g
checkPlayerProjectileCollision g@(GameState os p@(Player h pos pdir ps) s t d _) pr@(Projectile (x, y) dir) o@(Mine oh op@(ex, ey))
                                                                    | ((x + 10) > (ex - 10)) && ((y + 10) > (ey - 10)) && ((x - 10) > (ex + 10)) && ((y - 10) > (ey + 10)) = GameState ((Mine (oh - 1) op) : (filter (\x -> x /= o) os)) (Player h pos pdir (filter (\x -> x /= pr) ps)) s t d False
                                                                    | otherwise = g
checkPlayerProjectileCollision g _ _ = g

movePlayer :: Player -> Player
movePlayer (Player h (x, y) (dx, dy) ps) = Player h (x + dx, y + dy) (0.94 * dx, 0.94 * dy) ps

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