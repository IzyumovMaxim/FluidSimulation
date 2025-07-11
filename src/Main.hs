module Main where

import Types
import Physics
import Render
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..), MouseButton(..))

-- Initial world state with denser particles
initialWorld :: World
initialWorld = World
  { particles = [ Particle (x, y) (0,0) 1000 0
             | x <- [-50, -40..50]   -- Smaller step (2 units)
             , y <- [80, 82..140]    -- Smaller step (2 units)
             , (x*x + (y-110)*(y-110)) <= 50*50 ]
  , gravity   = (0, -9.8)
  , mass      = 0.2
  , rho0      = 1000
  , stiffness = 2500
  , viscosity = 10
  , h         = 15
  , mousePos  = (0,0)
  , mouseDown = False
  }

-- Two-pass simulation update with spatial grid
updateWorld :: Float -> World -> World
updateWorld _ world =
  let allParticles = particles world
      grid = buildGrid (h world) allParticles
      ps1 = map (computeDensityPressure world grid) allParticles
      tempWorld = world { particles = ps1 }
      ps2 = map (updatePositionVelocity tempWorld grid) ps1
  in world { particles = ps2 }

-- Handle mouse events with explicit pattern matching
eventHandler :: Event -> World -> World
eventHandler (EventMotion pos) world = 
  world { mousePos = pos }

eventHandler (EventKey (MouseButton button) Down _ pos) world
  | button == LeftButton = world { mouseDown = True, mousePos = pos }

eventHandler (EventKey (MouseButton button) Up _ pos) world
  | button == LeftButton = world { mouseDown = False, mousePos = pos }

eventHandler _ world = world

main :: IO ()
main = do
  let particleCount = length (particles initialWorld)
  putStrLn $ "Starting simulation with " ++ show particleCount ++ " particles"
  putStrLn "Click and drag to interact with the fluid"
  play
    (InWindow "SPH Fluid" (800, 800) (800, 800))
    black  
    300
    initialWorld
    renderWorld
    eventHandler
    updateWorld