module GameState where
    
import Objects
import Graphics.Gloss

data GameState = GameState {obstacles :: [Obstacle], player :: Player, score :: Int, timer :: Float, difficulty :: Int}