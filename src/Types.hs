module Types where

-- | 2D vector type for positions, velocities, and forces
-- Represents (x, y) coordinates or vector components
type Vector2 = (Float, Float)

-- | Individual particle in the fluid simulation
-- Each particle carries its own state and computed SPH properties
data Particle = Particle
  { position :: Vector2
  , velocity :: Vector2
  , density  :: Float
  , pressure :: Float
  } deriving (Show, Eq)

-- | Complete simulation world state
-- Contains all particles and global simulation parameters
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

-- | Generate initial particle distribution
-- Creates a circular drop of particles positioned above the container center
-- This provides a visually interesting starting configuration that demonstrates
-- fluid behavior as the drop falls and spreads
generateInitialParticles :: World -> [Particle]
generateInitialParticles world = 
  [ Particle (x, y) (0,0) 0 0
   | x <- [-50, -45..50]   -- Step 2
   , y <- [80, 85..140]    -- Step 2
   , (x*x + (y-110)*(y-110)) <= 50*50 ]
