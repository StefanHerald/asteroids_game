module GameState where
    
import Objects

data GameState = GameState {obstacles :: [Obstacles], player :: Player, score :: Score, timer :: Timer, difficulty :: Int}