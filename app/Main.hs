module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import GameState
--the world is 600 x 600
main :: IO ()
main = playIO (InWindow "Asteroids" (600, 600) (0, 0)) black 20 initialGameState view input step

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure (GameState _ _ _ timer _) = color red (text(show timer))

input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate) 

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char c) _ _ _ ) gstate = gstate

step :: Float -> GameState -> IO GameState
step secs gstate = return ( gstate{timer = timer gstate + secs })