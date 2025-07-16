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
  }

-- | Generate initial particle distribution
generateInitialParticles :: World -> [Particle]
generateInitialParticles world =
  [ Particle (x, y) (0,0) 0 0
  | x <- [-50, -45..50]
  , y <- [80, 85..140]
  , (x*x + (y-110)*(y-110)) <= 50*50 ]