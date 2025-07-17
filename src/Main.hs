module Main where

import Types
import Physics
import Render
import Level
import Graphics.Gloss (Display(InWindow), black, Picture(..))
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.Gloss.Interface.Pure.Game (play, Event(..), Key(..), KeyState(..), MouseButton(..), SpecialKey(..), Modifiers(..))
import qualified Data.Vector as V
import Data.Bits (Bits(shiftL))


-- | Initial world state with optimized parameters
initialWorld :: Picture -> World
initialWorld swampyPic = 
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
               -- New puzzle game fields
               , currentLevel = Nothing
               , gameState = Playing
               , collectedStars = 0
               , waterInGoal = False
               , swampyImg = swampyPic  
               }
  in w { particles = generateInitialParticles w }

-- | Optimized simulation update with puzzle game logic
updateWorld :: Float -> World -> World
updateWorld dt world =
  case scene world of
    PuzzleLevel -> updatePuzzleWorld world
    _ -> updateFluidWorld dt world

-- | Update puzzle game world
updatePuzzleWorld :: World -> World
updatePuzzleWorld world =
  let -- Update particle physics
      particleVector = V.fromList (particles world)
      grid = buildOptimizedGrid (h world) particleVector
      ps1 = computeDensityPressureParallel world grid particleVector
      tempWorld = world { particles = V.toList ps1 }
      tempVector = V.fromList (particles tempWorld)
      ps2 = updatePositionVelocityParallel tempWorld grid tempVector
      
      -- Update game state
      worldWithParticles = tempWorld { particles = V.toList ps2 }
      worldWithCollectibles = updateCollectibles worldWithParticles
      finalWorld = updateGameState worldWithCollectibles
  in finalWorld

-- | Update regular fluid simulation world
updateFluidWorld :: Float -> World -> World
updateFluidWorld dt world =
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


-- | Enhanced event handler with puzzle game controls
eventHandler :: Event -> World -> World
eventHandler (EventMotion pos) world = 
  world { mousePos = pos }

eventHandler (EventKey (MouseButton button) Down _ pos) world
  | button == LeftButton = 
      case scene world of
        PuzzleLevel -> handleDigging (world { mouseDown = True, mousePos = pos }) pos
        _ -> world { mouseDown = True, mousePos = pos }

eventHandler (EventKey (MouseButton button) Up _ pos) world
  | button == LeftButton = world { mouseDown = False, mousePos = pos }

-- Reset controls
eventHandler (EventKey (Char 'r') Down _ _) world = 
  case scene world of
    PuzzleLevel -> resetLevel world
    _ -> world { particles = generateInitialParticles world }

eventHandler (EventKey (Char 'R') Down _ _) world = 
  case scene world of
    PuzzleLevel -> resetLevel world
    _ -> world { particles = generateInitialParticles world }

-- Scene switching
eventHandler (EventKey (Char '1') Down _ _) world = 
  let newWorld = world { scene = Square }
  in newWorld { particles = generateInitialParticles newWorld }

eventHandler (EventKey (Char '2') Down _ _) world = 
  let newWorld = world { scene = Hourglass }
  in newWorld { particles = generateInitialParticles newWorld }

eventHandler (EventKey (Char '3') Down _ _) world = 
  let newWorld = world { scene = Ball }
  in newWorld { particles = generateInitialParticles newWorld }

eventHandler (EventKey (Char '4') Down _ _) world = 
  let newWorld = world { scene = Windmill }
  in newWorld { particles = generateInitialParticles newWorld }
  
eventHandler (EventKey (Char '5') Down _ _) world = 
  initializePuzzleLevel world

-- Physics parameter controls (only for non-puzzle modes)
eventHandler (EventKey (SpecialKey KeyUp) Down mods _) world 
  | scene world /= PuzzleLevel = 
      let step = if shift mods == Down then 5.0 else 0.5
          (gx, gy) = gravity world
      in world { gravity = (gx, gy + step) }

eventHandler (EventKey (SpecialKey KeyDown) Down mods _) world 
  | scene world /= PuzzleLevel = 
      let step = if shift mods == Down then 5.0 else 0.5
          (gx, gy) = gravity world
      in world { gravity = (gx, gy - step) }

eventHandler (EventKey (SpecialKey KeyRight) Down mods _) world 
  | scene world /= PuzzleLevel = 
      let step = if shift mods == Down then 5.0 else 0.5
          (gx, gy) = gravity world
      in world { gravity = (gx + step, gy) }

eventHandler (EventKey (SpecialKey KeyLeft) Down mods _) world 
  | scene world /= PuzzleLevel = 
      let step = if shift mods == Down then 5.0 else 0.5
          (gx, gy) = gravity world
      in world { gravity = (gx - step, gy) }

-- Parameter adjustment controls (only for fluid simulation)
eventHandler (EventKey (Char c) Down mods _) world
  | scene world /= PuzzleLevel = case c of
      't' -> let step = if shift mods == Down then 0.5 else 0.1 in world { mass = mass world + step }
      'T' -> let step = if shift mods == Down then 0.5 else 0.1 in world { mass = mass world + step }
      'g' -> let step = if shift mods == Down then 0.5 else 0.1 in world { mass = max 0.1 (mass world - step) }
      'G' -> let step = if shift mods == Down then 0.5 else 0.1 in world { mass = max 0.1 (mass world - step) }
      'y' -> let step = if shift mods == Down then 50 else 10 in world { rho0 = rho0 world + step }
      'Y' -> let step = if shift mods == Down then 50 else 10 in world { rho0 = rho0 world + step }
      'h' -> let step = if shift mods == Down then 50 else 10 in world { rho0 = max 1 (rho0 world - step) }
      'H' -> let step = if shift mods == Down then 50 else 10 in world { rho0 = max 1 (rho0 world - step) }
      'u' -> let step = if shift mods == Down then 100 else 25 in world { stiffness = stiffness world + step }
      'U' -> let step = if shift mods == Down then 100 else 25 in world { stiffness = stiffness world + step }
      'j' -> let step = if shift mods == Down then 100 else 25 in world { stiffness = max 1 (stiffness world - step) }
      'J' -> let step = if shift mods == Down then 100 else 25 in world { stiffness = max 1 (stiffness world - step) }
      'i' -> let step = if shift mods == Down then 5 else 1 in world { viscosity = viscosity world + step }
      'I' -> let step = if shift mods == Down then 5 else 1 in world { viscosity = viscosity world + step }
      'k' -> let step = if shift mods == Down then 5 else 1 in world { viscosity = max 0 (viscosity world - step) }
      'K' -> let step = if shift mods == Down then 5 else 1 in world { viscosity = max 0 (viscosity world - step) }
      'o' -> let step = if shift mods == Down then 3 else 1 in world { h = h world + step }
      'O' -> let step = if shift mods == Down then 3 else 1 in world { h = h world + step }
      'l' -> let step = if shift mods == Down then 3 else 1 in world { h = max 5 (h world - step) }
      'L' -> let step = if shift mods == Down then 3 else 1 in world { h = max 5 (h world - step) }
      'p' -> let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = surfaceTension world + step }
      'P' -> let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = surfaceTension world + step }
      ';' -> let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = max 0 (surfaceTension world - step) }
      ':' -> let step = if shift mods == Down then 0.005 else 0.001 in world { surfaceTension = max 0 (surfaceTension world - step) }
      'q' -> world { h = max 8 (h world - 1) }
      'Q' -> world { h = h world + 1 }
      'w' -> world { particles = take (length (particles world) - 10) (particles world) }
      'W' -> let newParticles = take 10 (generateInitialParticles world)
             in world { particles = particles world ++ newParticles }
      _ -> world

eventHandler _ world = world

-- | Main entry point with enhanced information
main :: IO ()
main = do
  putStrLn "Loading Swampy image..."
  maybeImg <- loadJuicyPNG "src/assets/i.png"
  case maybeImg of
    Nothing -> putStrLn "Failed to load swampy image!"
    Just _  -> putStrLn "Swampy image loaded successfully."

  let swampyImage = maybe Blank id maybeImg
      startingWorld = initialWorld swampyImage
      particleCount = length (particles startingWorld)
  -- putStrLn "Starting 'Where's My Water' SPH Game with " ++ show particleCount ++ " particles"
  putStrLn "Optimizations enabled:"
  putStrLn "  - Parallel computation (4 cores)"
  putStrLn "  - Vectorized operations"
  putStrLn "  - Optimized spatial grid"
  putStrLn "  - Reduced memory allocations"
  putStrLn ""
  putStrLn "Game modes:"
  putStrLn "  1 - Square fluid simulation"
  putStrLn "  2 - Hourglass fluid simulation"
  putStrLn "  3 - Ball fluid simulation"
  putStrLn "  4 - Windmill simulation"
  putStrLn "  5 - Puzzle Level (Where's My Water)"
  putStrLn ""
  putStrLn "Puzzle Game Controls:"
  putStrLn "  Click - Dig dirt blocks"
  putStrLn "  R - Reset level"
  putStrLn "  Goal: Collect all stars and get water to Swampy!"
  putStrLn ""
  putStrLn "Fluid Simulation Controls:"
  putStrLn "  R - Reset simulation"
  putStrLn "  Arrow keys - Adjust gravity"
  putStrLn "  T/G - Mass up/down"
  putStrLn "  U/J - Stiffness up/down"
  putStrLn "  I/K - Viscosity up/down"
  putStrLn "  P/; - Surface tension up/down"
  putStrLn "  Q/q - Smoothing radius (affects performance)"
  putStrLn "  W/w - Add/remove particles"
  putStrLn "  Hold Shift for larger adjustments"
  putStrLn "  Click and drag to interact with particles"
  
  play
    (InWindow "Fluid simulation" (800, 800) (100, 100))
    black  
    300
    startingWorld
    (\world -> renderWorld world swampyImage ) 
    eventHandler
    updateWorld