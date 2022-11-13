module GameState where
    
import Objects
import Graphics.Gloss

data GameState = GameState {obstacles :: [Obstacle], player :: Player, score :: Int, timer :: Float, difficulty :: Int, paused :: Bool}
                 deriving (Show)

instance ToFile GameState where
    toFile (GameState obs pl sc time dif _ ) = concat (map toFile obs) ++ "\n" 
                                               ++ toFile pl ++ "\n"
                                               ++ show sc ++ "\n"
                                               ++ show time ++ "\n"
                                               ++ show dif
initialGameState :: GameState
initialGameState = GameState [] initialPlayer 0 0 0 False

