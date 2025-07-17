module Types where

-- | 2D vector type for positions, velocities, and forces
type Vector2 = (Float, Float)

-- | Grid coordinates for level blocks
type GridCoord = (Int, Int)


-- | Individual particle in the fluid simulation
data Particle = Particle
  { position :: Vector2
  , velocity :: Vector2
  , density :: Float
  , pressure :: Float
  } deriving (Show, Eq)

-- | Different types of blocks in the level
data BlockType 
  = Empty         -- Air/empty space
  | Dirt          -- Diggable brown dirt
  | Stone         -- Permanent purple stone
  | WaterSource   -- Blue water spawning area
  | Goal          -- Swampy's bathtub area
  deriving (Show, Eq)

-- | Collectible items in the level
data Collectible = Star
  { starPos :: Vector2
  , collected :: Bool
  } deriving (Show, Eq)

-- | Game state for puzzle mechanics
data GameState 
  = Playing
  | Won
  | Lost
  deriving (Show, Eq)

-- | Level data structure
data Level = Level
  { levelGrid :: [[BlockType]]    -- 2D grid of blocks
  , gridSize :: (Int, Int)        -- (width, height) of grid
  , blockSize :: Float           -- Size of each block in world units
  , collectibles :: [Collectible] -- Stars to collect
  , waterSpawnArea :: (Vector2, Vector2) -- (min, max) spawn coordinates
  , goalArea :: (Vector2, Float)  -- (center, radius) of Swampy's tub
  } deriving (Show, Eq)

-- | Scene type for different container shapes
data Scene = Square | Hourglass | Ball | Windmill | PuzzleLevel  deriving (Show, Eq)

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
  , scene :: Scene             -- Current scene
  , windmillAngle :: Float
  , windmillSpeed :: Float 
    -- Puzzle game fields
  , currentLevel :: Maybe Level -- Current puzzle level
  , gameState :: GameState     -- Current game state
  , collectedStars :: Int      -- Number of stars collected
  , waterInGoal :: Bool        -- Is water reaching Swampy?
  }

-- | Check if point is inside hourglass shape
insideHourglass :: Vector2 -> Bool
insideHourglass (x, y)
  | y > 0 = abs x < (180 - y * 180 / 180)  -- Upper triangle
  | y < 0 = abs x < (180 + y * 180 / 180)  -- Lower triangle
  | otherwise = abs x < 10                  -- Narrow passage
  

-- | Generate initial particle distribution based on scene
generateInitialParticles :: World -> [Particle]
generateInitialParticles world =
  case scene world of
    Square ->
      [ Particle (x, y) (0,0) 0 0
      | x <- [-50, -45..50]
      , y <- [80, 85..140]
      , (x*x + (y-110)*(y-110)) <= 50*50 ]
    
    Hourglass ->
      [ Particle (x, y) (0,0) 0 0
      | x <- [-25, -17..25]
      , y <- [40, 45..175]
      , (x*x + (y-110)*(y-110)) <= 50*50 ]


    Ball -> 
      [ Particle (x, y) (0,0) 0 0
      | x <- [-50, -45..50]
      , y <- [80, 85..140]
      , (x*x + (y-110)*(y-110)) <= 50*50 ]

    Windmill ->
      [ Particle (x, y) (0,0) 0 0
      | x <- [-50, -45..50]
      , y <- [80, 85..140]
      , (x*x + (y-110)*(y-110)) <= 50*50 
      , not (inMillArea (x,y)) ]

    PuzzleLevel ->
      case currentLevel world of
        Nothing -> []
        Just level -> generateLevelParticles level
      
    where
      inMillArea (x,y) = 
        let dist = sqrt (x^2 + y^2)
        in dist < 60  -- Исключаем область мельницы

    
      
-- | Generate particles for puzzle level (spawn from water source)
generateLevelParticles :: Level -> [Particle]
generateLevelParticles level =
  let ((minX, minY), (maxX, maxY)) = waterSpawnArea level
      spacing = 4.0
      xCoords = [minX, minX + spacing .. maxX]
      yCoords = [minY, minY + spacing .. maxY]
  in [ Particle (x, y) (0, 0) 0 0 
     | x <- xCoords
     , y <- yCoords
     ]

-- | Convert world coordinates to grid coordinates
worldToGrid :: Level -> Vector2 -> GridCoord
worldToGrid level (x, y) = 
  let (gridW, gridH) = gridSize level
      blockSz = blockSize level
      -- Assume grid is centered at origin
      gridX = floor ((x + fromIntegral gridW * blockSz / 2) / blockSz)
      gridY = floor ((y + fromIntegral gridH * blockSz / 2) / blockSz)
  in (gridX, gridY)

-- | Convert grid coordinates to world coordinates (center of block)
gridToWorld :: Level -> GridCoord -> Vector2
gridToWorld level (gx, gy) =
  let (gridW, gridH) = gridSize level
      blockSz = blockSize level
      worldX = fromIntegral gx * blockSz - fromIntegral gridW * blockSz / 2 + blockSz / 2
      worldY = fromIntegral gy * blockSz - fromIntegral gridH * blockSz / 2 + blockSz / 2
  in (worldX, worldY)

-- | Get block type at grid coordinates
getBlockAt :: Level -> GridCoord -> BlockType
getBlockAt level (gx, gy) =
  let (gridW, gridH) = gridSize level
      grid = levelGrid level
  in if gx >= 0 && gx < gridW && gy >= 0 && gy < gridH
     then (grid !! gy) !! gx
     else Stone  -- Out of bounds = solid wall

-- | Set block type at grid coordinates (for digging)
setBlockAt :: Level -> GridCoord -> BlockType -> Level
setBlockAt level (gx, gy) newBlock =
  let (gridW, gridH) = gridSize level
      grid = levelGrid level
  in if gx >= 0 && gx < gridW && gy >= 0 && gy < gridH
     then level { levelGrid = updateGrid grid gy gx newBlock }
     else level
  where
    updateGrid :: [[a]] -> Int -> Int -> a -> [[a]]
    updateGrid grid row col newVal =
      let (beforeRows, targetRow:afterRows) = splitAt row grid
          (beforeCols, _:afterCols) = splitAt col targetRow
          newRow = beforeCols ++ [newVal] ++ afterCols
      in beforeRows ++ [newRow] ++ afterRows

-- | Sample level data (matching the image you showed)
sampleLevel :: Level
sampleLevel = Level
  { levelGrid = 
      [ [Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone]
      , [Stone, WaterSource, WaterSource, WaterSource, WaterSource, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone]
      , [Stone, WaterSource, WaterSource, WaterSource, WaterSource, Dirt, Dirt, Dirt, Dirt, Stone, Stone, Stone, Stone, Stone, Stone, Stone]
      , [Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone]
      , [Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone]
      , [Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone]
      , [Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone]
      , [Stone, Stone, Stone, Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone, Stone, Stone, Stone]
      , [Stone, Stone, Stone, Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone, Stone, Stone, Stone]
      , [Stone, Stone, Stone, Stone, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone, Stone, Stone, Stone]
      , [Stone,  Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone]
      , [Stone,  Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Dirt, Stone]
      , [Stone, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Stone, Stone, Stone, Stone, Stone, Stone, Stone]
      , [Stone, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Goal, Stone, Stone, Stone, Stone, Stone, Stone, Stone]
      , [Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone, Stone]
      ]
  , gridSize = (16, 15)
  , blockSize = 25.0
  , collectibles = 
      [ Star (50, 50) False    -- Star positions based on image
      , Star (50, -25) False
      , Star (50, -100) False
      ]
  , waterSpawnArea = ((-150, 150), (150, 175))  -- Top area where water spawns
  , goalArea = ((-150, -150), 50)  -- Swampy's tub in bottom left
}

