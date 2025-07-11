module Main where

import Types
import Physics
import Render
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..))

-- | Initial world state with default physical parameters
-- Creates a world with standard fluid properties similar to water
initialWorld :: World
initialWorld = 
  let w = World { particles = []           -- Start with empty particle list
               , gravity   = (0, -9.8)     -- Earth-like gravity (m/s²)
               , mass      = 0.2           -- Particle mass (kg)
               , rho0      = 1000          -- Rest density (kg/m³, water-like)
               , stiffness = 2500          -- Pressure stiffness coefficient
               , viscosity = 10            -- Viscosity coefficient
               , h         = 15            -- Smoothing radius (affects neighbor search)
               , mousePos  = (0,0)         -- Current mouse position
               , mouseDown = False         -- Mouse button state
               }
  -- Generate initial particle distribution after world creation
  in w { particles = generateInitialParticles w }

-- | Main simulation update function using two-pass SPH algorithm
-- This implements the standard SPH method:
-- Pass 1: Compute density and pressure for all particles
-- Pass 2: Compute forces and update positions/velocities
updateWorld :: Float -> World -> World
updateWorld _ world =
  let allParticles = particles world
      -- Build spatial grid for efficient neighbor finding
      grid = buildGrid (h world) allParticles

      -- First pass: compute density and pressure for each particle
      ps1 = map (computeDensityPressure world grid) allParticles
      
      -- Create temporary world state with updated densities/pressures
      tempWorld = world { particles = ps1 }
      
      -- Second pass: compute forces and update positions/velocities
      -- Uses the density/pressure values computed in the first pass
      ps2 = map (updatePositionVelocity tempWorld grid) ps1
  in world { particles = ps2 }

-- | Event handler for user input
-- Supports mouse interaction and keyboard controls for simulation parameters
eventHandler :: Event -> World -> World

-- Mouse events
eventHandler (EventMotion pos) world = 
  world { mousePos = pos }

eventHandler (EventKey (MouseButton button) Down _ pos) world
  | button == LeftButton = world { mouseDown = True, mousePos = pos }

eventHandler (EventKey (MouseButton button) Up _ pos) world
  | button == LeftButton = world { mouseDown = False, mousePos = pos }

-- Key events
eventHandler (EventKey (Char 'r') Down _ _) world = 
  world { particles = generateInitialParticles world }

eventHandler (EventKey (Char 'R') Down _ _) world = 
  world { particles = generateInitialParticles world }

-- Gravity control
eventHandler (EventKey (SpecialKey KeyUp) Down _ _) world = 
  world { gravity = (gx, gy + 0.5) } where (gx, gy) = gravity world
eventHandler (EventKey (SpecialKey KeyDown) Down _ _) world = 
  world { gravity = (gx, gy - 0.5) } where (gx, gy) = gravity world
eventHandler (EventKey (SpecialKey KeyRight) Down _ _) world = 
  world { gravity = (gx + 0.5, gy) } where (gx, gy) = gravity world
eventHandler (EventKey (SpecialKey KeyLeft) Down _ _) world = 
  world { gravity = (gx - 0.5, gy) } where (gx, gy) = gravity world

-- Mass control
eventHandler (EventKey (Char 't') Down _ _) world = 
  world { mass = mass world + 0.1 }
eventHandler (EventKey (Char 'g') Down _ _) world = 
  world { mass = max 0.1 (mass world - 0.1) }

-- Density control
eventHandler (EventKey (Char 'y') Down _ _) world = 
  world { rho0 = rho0 world + 10 }
eventHandler (EventKey (Char 'h') Down _ _) world = 
  world { rho0 = max 1 (rho0 world - 10) }

-- Stiffness control
eventHandler (EventKey (Char 'u') Down _ _) world = 
  world { stiffness = stiffness world + 100 }
eventHandler (EventKey (Char 'j') Down _ _) world = 
  world { stiffness = max 1 (stiffness world - 100) }

-- Viscosity control
eventHandler (EventKey (Char 'i') Down _ _) world = 
  world { viscosity = viscosity world + 1 }
eventHandler (EventKey (Char 'k') Down _ _) world = 
  world { viscosity = max 0 (viscosity world - 1) }

-- Smoothing radius control
eventHandler (EventKey (Char 'o') Down _ _) world = 
  world { h = h world + 1 }
eventHandler (EventKey (Char 'l') Down _ _) world = 
  world { h = max 5 (h world - 1) }

-- Ignore other events
eventHandler _ world = world

-- | Main entry point
-- Sets up the Gloss window and starts the simulation loop
main :: IO ()
main = do
  let particleCount = length (particles initialWorld)
  putStrLn $ "Starting simulation with " ++ show particleCount ++ " particles"
  putStrLn "Controls:"
  putStrLn "  R - Reset simulation"
  putStrLn "  Arrow keys - Control gravity"
  putStrLn "  T/G - Increase/Decrease mass"
  putStrLn "  Y/H - Increase/Decrease density"
  putStrLn "  U/J - Increase/Decrease stiffness"
  putStrLn "  I/K - Increase/Decrease viscosity"
  putStrLn "  O/L - Increase/Decrease smoothing radius"
  putStrLn "Click and drag to interact with the fluid"
  
  play
    (InWindow "SPH Fluid" (800, 800) (800, 800))
    black  
    300
    initialWorld
    renderWorld
    eventHandler
    updateWorld
