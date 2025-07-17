module Types where

-- | 2D vector type for positions, velocities, and forces
type Vector2 = (Float, Float)

-- | Individual particle in the fluid simulation
data Particle = Particle
  { position :: Vector2
  , velocity :: Vector2
  , density :: Float
  , pressure :: Float
  } deriving (Show, Eq)

-- | Scene type for different container shapes
data Scene = Square | Hourglass | Ball deriving (Show, Eq)

-- | Complete simulation world state
data World = World
  { particles :: [Particle]
  , gravity :: Vector2
  , mass :: Float
  , rho0 :: Float              -- Rest density
  , stiffness :: Float         -- Pressure coefficient
  , viscosity :: Float         -- Viscosity coefficient
  , h :: Float                 -- Smoothing radius
  , surfaceTension :: Float    -- Surface tension coefficient
  , mousePos :: Vector2        -- Mouse position
  , mouseDown :: Bool          -- Mouse state
  , scene :: Scene             -- Current scene
  }

-- | Check if point is inside hourglass shape
insideHourglass :: Vector2 -> Bool
insideHourglass (x, y)
  | y > 0 = abs x < (180 - y * 180 / 180)  -- Upper triangle
  | y < 0 = abs x < (180 + y * 180 / 180)  -- Lower triangle
  | otherwise = abs x < 10                  -- Narrow passage
  

-- | Generate initial particle distribution based on scene
generateInitialParticles :: World -> [Particle]
generateInitialParticles world =
  case scene world of
    Square ->
      [ Particle (x, y) (0,0) 0 0
      | x <- [-50, -45..50]
      , y <- [80, 85..140]
      , (x*x + (y-110)*(y-110)) <= 50*50 ]
    
    Hourglass ->
      [ Particle (x, y) (0,0) 0 0
      | x <- [-25, -17..25]
      , y <- [40, 45..175]
      , (x*x + (y-110)*(y-110)) <= 50*50 ]
      
    Ball -> 
      [ Particle (x, y) (0,0) 0 0
      | x <- [-50, -45..50]
      , y <- [80, 85..140]
      , (x*x + (y-110)*(y-110)) <= 50*50 
      , not (inBall (x,y)) ]
  where
    inBall (x,y) = (x)^2 + (y)^2 <= 50^2
