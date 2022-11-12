module Objects where
type Dir      = (Float, Float)
type Pos      = (Float, Float)
type ShootDir = (Float, Float)
type Health   = Int
data Obstacle = Enemy Health Pos Dir ShootDir
                | Asteroid Health Pos Dir
                | Mine Health Pos
                | Projectile Pos Dir
                deriving (Show)
data Player   = Player Health Pos Dir ShootDir
                deriving (Show)
initialPlayer :: Player
initialPlayer = Player 3 (300, 300) (0,0) (0,0)