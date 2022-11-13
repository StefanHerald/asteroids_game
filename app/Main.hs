module Main where
import Graphics.Gloss 
import Graphics.Gloss.Interface.IO.Game
import GameState
import Objects

main :: IO ()
main = playIO (InWindow "Asteroids" (600, 600) (0, 0)) --window name: asteroids, 600x600
               black --background
               20  --framerate
               initialGameState --the first gamestate 
               view --how to draw everything
               input --how to handle input from the keyboard
               step  -- advance the rest of the world

view :: GameState -> IO Picture --impure function returning the view
view = return . viewPure

viewPure :: GameState -> Picture --pure function for the view
viewPure (GameState _ _ _ timer _) = color red (rotateAndTranslate (300, 0) (0, 0) (text(show timer)))

input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate) 

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char c) _ _ _ ) gstate = gstate
handleInput _                          gstate = gstate 

step :: Float -> GameState -> IO GameState --secs is truly the amount of secs
step secs gstate = return ( gstate{timer = timer gstate + secs })