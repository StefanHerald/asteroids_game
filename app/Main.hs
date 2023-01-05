module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
import GameState
import Objects
import PausingAndLoading
import Collision
import Movement
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
step secs gstate@(GameState os p s t d False) = do
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
step _ g = return g
                                    
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



--moving objects and deleting them
updateGamestate :: GameState -> GameState
updateGamestate g@(GameState os p@(Player h pos dir ps) s t d False) = applyApplyChain (applyChain checkPlayerCollision (GameState (map (moveObjects p) os) (movePlayer (Player h pos dir (map (moveObjects p) (deleteOutOfBounds ps)))) s t d False) os) ps os
updateGamestate g = g

applyChain :: (GameState -> Obstacle -> GameState) -> GameState -> [Obstacle] -> GameState
applyChain f a (b : []) = f a b
applyChain f a (b : bs) = applyChain f (f a b) bs
applyChain _ a [] = a

applyApplyChain :: GameState -> [Obstacle] -> [Obstacle] -> GameState
applyApplyChain a [] _ = a
applyApplyChain a (p : []) os = applyChain (checkPlayerProjectileCollision p) a os
applyApplyChain a (p : ps) os = applyApplyChain (applyChain (checkPlayerProjectileCollision p) a os) ps os


deleteOutOfBounds :: [Obstacle] -> [Obstacle]
deleteOutOfBounds ps = filter (\(Projectile (x, y) dir) -> x < 300 && y < 300 && x > -300 && y > -300) ps


