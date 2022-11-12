module Objects where
type Dir      = (Float, Float)
type Pos      = (Float, Float)
type ShootDir = (Float, Float)
type Health   = Int
data Obstacle = Enemy Health Pos Dir ShootDir
                | Asteroid Health Pos Dir
                | Mine Health Pos
                | Projectile Pos Dir
data Player   = Player Health Pos Dir ShootDir
    