module Level where

import Types
import Data.List (find)

-- | Handle mouse click for digging mechanics
handleDigging :: World -> Vector2 -> World
handleDigging world pos = 
  case currentLevel world of
    Nothing -> world
    Just level -> 
      let gridCoord = worldToGrid level pos
          currentBlock = getBlockAt level gridCoord
          newWorld = case currentBlock of
            Dirt -> 
              let newLevel = setBlockAt level gridCoord Empty
              in world { currentLevel = Just newLevel }
            _ -> world  -- Can't dig stone, empty space, etc.
      in newWorld

-- | Check if a position is blocked by solid geometry
isPositionBlocked :: World -> Vector2 -> Bool
isPositionBlocked world pos =
  case currentLevel world of
    Nothing -> False
    Just level ->
      let gridCoord = worldToGrid level pos
          blockType = getBlockAt level gridCoord
      in blockType == Stone || blockType == Dirt

-- | Check collision between particle and level geometry
checkLevelCollision :: World -> Particle -> (Vector2, Vector2) -> (Vector2, Vector2)
checkLevelCollision world particle (newPos, newVel) =
  case currentLevel world of
    Nothing -> (newPos, newVel)
    Just level ->
      let (x, y) = newPos
          (vx, vy) = newVel
          bs  = blockSize level
          
          -- Check collision in multiple directions
          leftBlocked = isPositionBlocked world (x - bs /2, y)
          rightBlocked = isPositionBlocked world (x + bs /2, y)
          topBlocked = isPositionBlocked world (x, y + bs /2)
          bottomBlocked = isPositionBlocked world (x, y - bs /2)
          
          -- Adjust position and velocity based on collisions
          (finalX, finalVx) = 
            if leftBlocked && vx < 0
            then (x + bs /2, -vx * 0.5)
            else if rightBlocked && vx > 0
            then (x - bs /2, -vx * 0.5)
            else (x, vx)
            
          (finalY, finalVy) = 
            if bottomBlocked && vy < 0
            then (y + bs /2, -vy * 0.5)
            else if topBlocked && vy > 0
            then (y - bs /2, -vy * 0.5)
            else (y, vy)
            
      in ((finalX, finalY), (finalVx, finalVy))

-- | Update collectibles when particles touch them
updateCollectibles :: World -> World
updateCollectibles world =
  case currentLevel world of
    Nothing -> world
    Just level ->
      let worldParticles = particles world
          levelCollectibles = collectibles level
          
          -- Check each collectible against all particles
          updatedCollectibles = map (checkStarCollection worldParticles) levelCollectibles
          
          -- Update level and world
          newLevel = level { collectibles = updatedCollectibles }
          collectedCount = length $ filter collected updatedCollectibles
          
      in world { currentLevel = Just newLevel
               , collectedStars = collectedCount
               }

-- | Check if a star should be collected by any particle
checkStarCollection :: [Particle] -> Collectible -> Collectible
checkStarCollection particles star
  | collected star = star  -- Already collected
  | otherwise = 
      let starPosition = starPos star
          collectionRadius = 20.0
          isNearParticle = any (\p -> distance p starPosition < collectionRadius) particles
      in if isNearParticle 
         then star { collected = True }
         else star
  where
    distance :: Particle -> Vector2 -> Float
    distance p (sx, sy) = 
      let (px, py) = position p
      in sqrt ((px - sx)^2 + (py - sy)^2)

-- | Check if water has reached Swampy's goal area
checkWaterInGoal :: World -> Bool
checkWaterInGoal world =
  case currentLevel world of
    Nothing -> False
    Just level ->
      let worldParticles = particles world
          (goalCenter, goalRadius) = goalArea level
          
          -- Check if any particle is in the goal area
          isParticleInGoal p = 
            let (px, py) = position p
                (gx, gy) = goalCenter
                distToGoal = sqrt ((px - gx)^2 + (py - gy)^2)
            in distToGoal <= goalRadius
            
      in any isParticleInGoal worldParticles

-- | Update game state based on win/lose conditions
updateGameState :: World -> World
updateGameState world =
  case scene world of
    PuzzleLevel ->
      case currentLevel world of
        Nothing -> world
        Just level ->
          let waterInGoal = checkWaterInGoal world
              allStarsCollected = all collected (collectibles level)
              
              newGameState = 
                if waterInGoal && allStarsCollected
                then Won
                else if gameState world == Playing
                then Playing
                else gameState world
                
          in world { gameState = newGameState
                   , waterInGoal = waterInGoal
                   }
    _ -> world

-- | Initialize world with puzzle level
initializePuzzleLevel :: World -> World
initializePuzzleLevel world = 
  let baseLevel = sampleLevel
      newWorld = world { scene = PuzzleLevel
                       , currentLevel = Just sampleLevel
                       , initialLevel = Just baseLevel
                       , gameState = Playing
                       , collectedStars = 0
                       , waterInGoal = False
                       }
  in newWorld { particles = generateInitialParticles newWorld }

-- | Reset current level
resetLevel :: World -> World
resetLevel world = 
  case initialLevel world of
    Nothing -> world
    Just level ->
      let newWorld = world { currentLevel = Just level
                           , gameState = Playing
                           , collectedStars = 0
                           , waterInGoal = False
                           }
      in newWorld { particles = generateInitialParticles newWorld }

-- | Get block color for rendering
getBlockColor :: BlockType -> (Float, Float, Float)
getBlockColor blockType =
  case blockType of
    Empty -> (0, 0, 0)         -- Transparent/black
    Dirt -> (0.5, 0.4, 0.3)    -- Brown
    Stone -> (0.3, 0.3, 0.3)   -- Dark Grey
    WaterSource -> (0.3, 0.7, 1.0)  -- Light blue
    Goal -> (0.9, 0.9, 0.9)    -- Light Grey