module GameState where
    
import Objects
import Graphics.Gloss


data GameState = GameState {obstacles :: [Obstacles], player :: Player, score :: Score, timer :: Float, difficulty :: Int}