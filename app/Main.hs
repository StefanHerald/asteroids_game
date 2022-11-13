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
handleInput (EventKey (Char c) _ _ _ ) gstate = gstate
handleInput _                          gstate = gstate 


step :: Float -> GameState -> IO GameState --secs is truly the amount of secs
step secs gstate = return ( gstate{timer = timer gstate + secs })