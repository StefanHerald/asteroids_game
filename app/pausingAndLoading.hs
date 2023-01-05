module PausingAndLoading where

import GameState
import Objects
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment
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