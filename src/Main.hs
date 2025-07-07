module Main where

import Types
import Physics
import Render
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.Pure.Game (play, Event)

-- Initial world state with denser particles, centered in container
initialWorld :: World
initialWorld = World
  { particles = [ Particle (x, y) (0,0) 1000 0
             | x <- [-50,-49..50]
             , y <- [80,81..140]
             , (x*x + (y-110)*(y-110)) <= 50*50 ] -- positioned high to let it fall
  , gravity   = (0, -9.8)
  , mass      = 0.2
  , rho0      = 1000
  , stiffness = 2500    -- incompressibility
  , viscosity = 10      -- moderate viscosity
  , h         = 10      -- kernel radius
  }

-- Advance simulation by ignoring dt (using internal step)
updateWorld :: Float -> World -> World
updateWorld _ world =
  world { particles = map (updateParticle world) (particles world) }

-- Ignore events
eventHandler :: Event -> World -> World
eventHandler _ w = w

main :: IO ()
main = play
  (InWindow "SPH Fluid" (800, 800) (800, 800))
  black  
  60     -- fps
  initialWorld
  renderWorld
  eventHandler
  updateWorld