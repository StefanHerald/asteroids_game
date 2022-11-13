module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import GameState
import Objects
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
view gstate = return (viewPure gstate)

viewPure :: GameState -> Picture --pure function for the view
viewPure (GameState obs pl sc time dif b) | b  = pictures ((dimAll full) ++ addToPaused)
                                          | otherwise = pictures full
    where
        dimAll :: [Picture] -> [Picture]
        dimAll []              = []
        dimAll((Color c p):xs) = (Color (dim c) p) : dimAll xs
        dimAll (x:xs)          = x:xs
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
input e gstate | paused gstate = handlePause e gstate
               | otherwise = return (handleInput e gstate) 

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (dx, dy - 5) ps) s t d False
handleInput (EventKey (Char 'a') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (dx - 5, dy) ps) s t d False
handleInput (EventKey (Char 's') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (dx, dy + 5) ps) s t d False
handleInput (EventKey (Char 'd') _ _ _ ) (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (dx + 5, dy) ps) s t d False
handleInput (EventKey (SpecialKey KeySpace) Down _ _) gstate = gstate{paused = True}
handleInput _                          (GameState os (Player h pos (dx, dy) ps) s t d False) = GameState os (Player h pos (0.75 * dx, 0.75 * dy) ps) s t d False
handleInput _ gstate = gstate
step :: Float -> GameState -> IO GameState --secs is truly the amount of secs
step secs gstate |  paused gstate = return gstate
                 |  otherwise     = return ( gstate{timer = timer gstate + secs })

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
    return ()

loadGame :: IO GameState
loadGame = do
    file <- readFile "../asteroids_game/saves/save"
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

--moving objects
updateGamestate :: GameState -> GameState
updateGamestate (GameState os p s t d False) = GameState (map moveObjects os) (movePlayer p) s t d False
updateGamestate gstate = gstate

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