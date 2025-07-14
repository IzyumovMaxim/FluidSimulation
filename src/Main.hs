module Main where

import Types
import Physics
import Render
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..), Modifiers(..))
import Data.Bits (Bits(shiftL))

-- | Initial world state with default physical parameters
-- Creates a world with standard fluid properties similar to water
initialWorld :: World
initialWorld = 
  let w = World { particles = []           -- Start with empty particle list
               , gravity   = (0, -20.0)     -- Earth-like gravity (m/s²)
               , mass      = 3.0           -- Particle mass (kg)
               , rho0      = 1000          -- Rest density (kg/m³, water-like)
               , stiffness = 3500          -- Pressure stiffness coefficient
               , viscosity = 80            -- Viscosity coefficient
               , h         = 11            -- Smoothing radius (affects neighbor search)
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


-- Restart simulation
eventHandler (EventKey (Char 'r') Down _ _) world = 
  world { particles = generateInitialParticles world }

eventHandler (EventKey (Char 'R') Down _ _) world = 
  world { particles = generateInitialParticles world }



-- Gravity control with Shift modifier
eventHandler (EventKey (SpecialKey KeyUp) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
  in world { gravity = (gx, gy + step) } where (gx, gy) = gravity world

eventHandler (EventKey (SpecialKey KeyDown) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
  in world { gravity = (gx, gy - step) } where (gx, gy) = gravity world

eventHandler (EventKey (SpecialKey KeyRight) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
  in world { gravity = (gx + step, gy) } where (gx, gy) = gravity world

eventHandler (EventKey (SpecialKey KeyLeft) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
  in world { gravity = (gx - step, gy) } where (gx, gy) = gravity world


-- Для буквенных клавиш - добавляем обработку как строчных, так и заглавных
eventHandler (EventKey (Char c) Down mods _) world
  | c `elem` ['t','T'] =  -- Mass increase
      let step = if shift mods == Down then 1.0 else 0.1
      in world { mass = mass world + step }
  
  | c `elem` ['g','G'] =  -- Mass decrease
      let step = if shift mods == Down then 1.0 else 0.1
      in world { mass = max 0.1 (mass world - step) }
  
  | c `elem` ['y','Y'] =  -- Density increase
      let step = if shift mods == Down then 100 else 10
      in world { rho0 = rho0 world + step }
  
  | c `elem` ['h','H'] =  -- Density decrease
      let step = if shift mods == Down then 100 else 10
      in world { rho0 = max 1 (rho0 world - step) }
  
  | c `elem` ['u','U'] =  -- Stiffness increase
      let step = if shift mods == Down then 1000 else 100
      in world { stiffness = stiffness world + step }
  
  | c `elem` ['j','J'] =  -- Stiffness decrease
      let step = if shift mods == Down then 1000 else 100
      in world { stiffness = max 1 (stiffness world - step) }
  
  | c `elem` ['i','I'] =  -- Viscosity increase
      let step = if shift mods == Down then 10 else 1
      in world { viscosity = viscosity world + step }
  
  | c `elem` ['k','K'] =  -- Viscosity decrease
      let step = if shift mods == Down then 10 else 1
      in world { viscosity = max 0 (viscosity world - step) }
  
  | c `elem` ['o','O'] =  -- Smoothing radius increase
      let step = if shift mods == Down then 10 else 1
      in world { h = h world + step }
  
  | c `elem` ['l','L'] =  -- Smoothing radius decrease
      let step = if shift mods == Down then 10 else 1
      in world { h = max 5 (h world - step) }
  

-- Ignore other events
eventHandler _ world = world

-- | Main entry point
-- Sets up the Gloss window and starts the simulation loop
main :: IO ()
main = do
  let particleCount = length (particles initialWorld)
  putStrLn $ "Starting simulation with " ++ show particleCount ++ " particles"
  
  play
    (InWindow "SPH Fluid" (800, 800) (800, 800))
    black  
    300
    initialWorld
    renderWorld
    eventHandler
    updateWorld
