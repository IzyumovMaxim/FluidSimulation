module Main where

import Types
import Physics
import Render
import Graphics.Gloss (Display(InWindow), black)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..), Modifiers(..))
import qualified Data.Vector as V
import Data.Bits (Bits(shiftL))

-- | Initial world state with optimized parameters
initialWorld :: World
initialWorld = 
  let w = World { particles = []
                , gravity   = (0, -15.0)
                , mass      = 2.0
                , rho0      = 1000
                , stiffness = 1000
                , viscosity = 15
                , h         = 8
                , surfaceTension = 0.005
                , mousePos  = (0,0)
                , mouseDown = False
                , scene     = Square
                , windmillAngle = 0
                , windmillSpeed = 1.5
               }
  in w { particles = generateInitialParticles w }


-- | Optimized simulation update using vectorized operations
updateWorld :: Float -> World -> World
updateWorld dt world =
  let -- Обновляем угол мельницы на основе времени и скорости
      newAngle = windmillAngle world + windmillSpeed world * dt
      
      -- Обновляем частицы
      particleVector = V.fromList (particles world)
      grid = buildOptimizedGrid (h world) particleVector
      ps1 = computeDensityPressureParallel world grid particleVector
      tempWorld = world { particles = V.toList ps1 }
      tempVector = V.fromList (particles tempWorld)
      ps2 = updatePositionVelocityParallel tempWorld grid tempVector
      
  in world { particles = V.toList ps2, windmillAngle = newAngle }

-- | Event handler with scene switching
eventHandler :: Event -> World -> World
eventHandler (EventMotion pos) world = 
  world { mousePos = pos }

eventHandler (EventKey (MouseButton button) Down _ pos) world
  | button == LeftButton = world { mouseDown = True, mousePos = pos }

eventHandler (EventKey (MouseButton button) Up _ pos) world
  | button == LeftButton = world { mouseDown = False, mousePos = pos }

eventHandler (EventKey (Char 'r') Down _ _) world = 
  world { particles = generateInitialParticles world }

eventHandler (EventKey (Char 'R') Down _ _) world = 
  world { particles = generateInitialParticles world }

eventHandler (EventKey (Char '1') Down _ _) world = 
  world { scene = Square, particles = generateInitialParticles world { scene = Square } }

eventHandler (EventKey (Char '2') Down _ _) world = 
  world { scene = Hourglass, particles = generateInitialParticles world { scene = Hourglass } }

eventHandler (EventKey (Char '3') Down _ _) world = 
  world { scene = Ball, particles = generateInitialParticles world { scene = Ball } }

eventHandler (EventKey (Char '4') Down _ _) world = 
  world { scene = Windmill, particles = generateInitialParticles world { scene = Windmill } }
  

eventHandler (EventKey (SpecialKey KeyUp) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
      (gx, gy) = gravity world
  in world { gravity = (gx, gy + step) }

eventHandler (EventKey (SpecialKey KeyDown) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
      (gx, gy) = gravity world
  in world { gravity = (gx, gy - step) }

eventHandler (EventKey (SpecialKey KeyRight) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
      (gx, gy) = gravity world
  in world { gravity = (gx + step, gy) }

eventHandler (EventKey (SpecialKey KeyLeft) Down mods _) world = 
  let step = if shift mods == Down then 5.0 else 0.5
      (gx, gy) = gravity world
  in world { gravity = (gx - step, gy) }

eventHandler (EventKey (Char c) Down mods _) world
  | c `elem` ['t','T'] = let step = if shift mods == Down then 0.5 else 0.1 in world { mass = mass world + step }
  | c `elem` ['g','G'] = let step = if shift mods == Down then 0.5 else 0.1 in world { mass = max 0.1 (mass world - step) }

  | c `elem` ['y','Y'] = let step = if shift mods == Down then 50 else 10 in world { rho0 = rho0 world + step }
  | c `elem` ['h','H'] = let step = if shift mods == Down then 50 else 10 in world { rho0 = max 1 (rho0 world - step) }

  | c `elem` ['u','U'] = let step = if shift mods == Down then 100 else 25 in world { stiffness = stiffness world + step }
  | c `elem` ['j','J'] = let step = if shift mods == Down then 100 else 25 in world { stiffness = max 1 (stiffness world - step) }

  | c `elem` ['i','I'] = let step = if shift mods == Down then 5 else 1 in world { viscosity = viscosity world + step }
  | c `elem` ['k','K'] = let step = if shift mods == Down then 5 else 1 in world { viscosity = max 0 (viscosity world - step) }

  | c `elem` ['o','O'] = let step = if shift mods == Down then 3 else 1 in world { h = h world + step }
  | c `elem` ['l','L'] = let step = if shift mods == Down then 3 else 1 in world { h = max 5 (h world - step) }

  | c `elem` ['p','P'] = let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = surfaceTension world + step }
  | c `elem` [';',':'] = let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = max 0 (surfaceTension world - step) }

  -- | c `elem` ['[','{'] = let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = surfaceTension world + step }
  -- | c `elem` ['\'','\"'] = let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = max 0 (surfaceTension world - step) }


eventHandler (EventKey (Char 'q') Down _ _) world = 
  world { h = max 8 (h world - 1) }

eventHandler (EventKey (Char 'Q') Down _ _) world = 
  world { h = h world + 1 }

eventHandler (EventKey (Char 'w') Down _ _) world = 
  world { particles = take (length (particles world) - 10) (particles world) }

eventHandler (EventKey (Char 'W') Down _ _) world = 
  let newParticles = take 10 (generateInitialParticles world)
  in world { particles = particles world ++ newParticles }

eventHandler _ world = world

-- | Main entry point with performance information
main :: IO ()
main = do
  let particleCount = length (particles initialWorld)
  putStrLn $ "Starting optimized SPH simulation with " ++ show particleCount ++ " particles"
  putStrLn "Controls:"
  putStrLn "  R - Reset simulation"
  putStrLn "  Arrow keys - Adjust gravity"
  putStrLn "  T/G - Mass up/down"
  putStrLn "  U/J - Stiffness up/down"
  putStrLn "  I/K - Viscosity up/down"
  putStrLn "  P/; - Surface tension up/down"
  putStrLn "  Q/q - Smoothing radius (affects performance)"
  putStrLn "  W/w - Add/remove particles"
  putStrLn "  1 - Switch to Square scene"
  putStrLn "  2 - Switch to Hourglass scene"
  putStrLn "  3 - Switch to Ball scene"
  putStrLn "  4 - Switch to Windmill scene"
  putStrLn "  Hold Shift for larger adjustments"
  putStrLn "  Click and drag to interact with particles"
  
  play
    (InWindow "Optimized SPH Fluid Simulation" (800, 800) (100, 100))
    black  
    300
    initialWorld
    renderWorld
    eventHandler
    updateWorld
