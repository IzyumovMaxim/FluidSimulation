module Types where

type Vector2 = (Float, Float)

data Particle = Particle
  { position :: Vector2
  , velocity :: Vector2
  , density  :: Float
  , pressure :: Float
  } deriving (Show, Eq)

data World = World
  { particles :: [Particle]
  , gravity   :: Vector2
  , mass      :: Float
  , rho0      :: Float  -- Rest density
  , stiffness :: Float  -- Pressure coefficient
  , viscosity :: Float  -- Viscosity coefficient
  , h         :: Float  -- Smoothing radius
  , mousePos  :: Vector2  -- Add mouse position
  , mouseDown :: Bool     -- Add mouse state
  }

-- Generate initial particles based on current world parameters
generateInitialParticles :: World -> [Particle]
generateInitialParticles world = 
  [ Particle (x, y) (0,0) 0 0
   | x <- [-50, -45..50]   -- Step 2
   , y <- [80, 85..140]    -- Step 2
   , (x*x + (y-110)*(y-110)) <= 50*50 ]
