module Main where

import Types
import Physics
import Render
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.Pure.Game (play, Event)

-- Initial world state with reduced particles
initialWorld :: World
initialWorld = World
  { particles = [ Particle (x, y) (0,0) 1000 0
             | x <- [-40, -20, 0, 20, 40]  -- Reduced resolution
             , y <- [100, 120, 140]         -- Reduced resolution
             , (x*x + (y-110)*(y-110)) <= 50*50 ]
  , gravity   = (0, -9.8)
  , mass      = 0.2
  , rho0      = 1000
  , stiffness = 2500
  , viscosity = 10
  , h         = 10
  }

-- Two-pass simulation update
updateWorld :: Float -> World -> World
updateWorld _ world =
  let -- First pass: compute all densities/pressures
      ps1 = map (computeDensityPressure world) (particles world)
      tempWorld = world { particles = ps1 }
      
      -- Second pass: update positions/velocities
      ps2 = map (updatePositionVelocity tempWorld) ps1
  in world { particles = ps2 }

-- Ignore events
eventHandler :: Event -> World -> World
eventHandler _ w = w

main :: IO ()
main = do
  putStrLn $ "Starting simulation with " ++ show (length (particles initialWorld)) ++ " particles"
  play
    (InWindow "SPH Fluid" (800, 800) (800, 800))
    black  
    60     -- fps
    initialWorld
    renderWorld
    eventHandler
    updateWorld
