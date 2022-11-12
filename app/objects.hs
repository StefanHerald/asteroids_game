module Objects where
    type Dir = Float Float
    type Pos = FLoat Float
    type Health = Int
    data Obstacle = Enemy Health Pos Dir
                  | Asteroid Health Pos Dir
                  | Mine Health Pos
                  | Projectile Pos Dir
