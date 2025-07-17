module Physics where

import Types
import Level (checkLevelCollision, isPositionBlocked)
import Data.List (foldl')
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Maybe (fromMaybe)
import Control.Parallel.Strategies
import Control.Parallel

-- | Optimized spatial grid using IntMap for better performance
type OptimizedGrid = IM.IntMap [Int]  -- Maps cell hash to particle indices

-- | Hash function for 2D coordinates to single integer
hashCoords :: Int -> Int -> Int
hashCoords x y = x * 73856093 + y * 19349663  -- Large primes for better distribution

-- | Build optimized spatial grid with particle indices
buildOptimizedGrid :: Float -> V.Vector Particle -> OptimizedGrid
buildOptimizedGrid hVal particles = 
  let indexed = V.imap (,) particles
      cellAssignments = V.map assignCell indexed
  in IM.fromListWith (++) (V.toList cellAssignments)
  where
    cellSize = hVal * 1.2
    assignCell (idx, p) = 
      let (x, y) = position p
          i = floor (x / cellSize)
          j = floor (y / cellSize)
          hash = hashCoords i j
      in (hash, [idx])

-- | Get neighbor indices using optimized grid
getNeighborIndices :: OptimizedGrid -> V.Vector Particle -> Int -> Float -> [Int]
getNeighborIndices grid particles idx hVal = 
  let p = particles V.! idx
      (x, y) = position p
      i0 = floor (x / cellSize)
      j0 = floor (y / cellSize)
      cellHashes = [hashCoords (i0+dx) (j0+dy) | dx <- [-1,0,1], dy <- [-1,0,1]]
      candidates = concatMap (\h -> fromMaybe [] (IM.lookup h grid)) cellHashes
  in filter (\nIdx -> nIdx /= idx && distance p (particles V.! nIdx) < hVal) candidates
  where
    cellSize = hVal * 1.2

-- | Parallel density and pressure computation
computeDensityPressureParallel :: World -> OptimizedGrid -> V.Vector Particle -> V.Vector Particle
computeDensityPressureParallel world grid particles = 
  let indices = [0 .. V.length particles - 1]
      chunkSize = max 1 (V.length particles `div` 4)
      chunks = chunksOf chunkSize indices
      processChunk chunk = map (computeDensityPressureForIndex world grid particles) chunk
      results = parMap rseq processChunk chunks
      flatResults = concat results
  in V.fromList flatResults

-- | Compute density and pressure for a single particle index
computeDensityPressureForIndex :: World -> OptimizedGrid -> V.Vector Particle -> Int -> Particle
computeDensityPressureForIndex world grid particles idx =
  let p = particles V.! idx
      neighborIndices = getNeighborIndices grid particles idx (h world)
      neighbors = map (particles V.!) neighborIndices
      rho = max 1.0 (computeDensity p neighbors (mass world) (h world))
      pVal = max 0 (stiffness world * ((rho / rho0 world) - 1))
  in p { density = rho, pressure = pVal }

-- | Optimized force computation with reduced allocations
data ForceAccumulator = ForceAccumulator 
  { fPressure :: {-# UNPACK #-} !Vector2
  , fViscosity :: {-# UNPACK #-} !Vector2
  , fSurface :: {-# UNPACK #-} !Vector2
  , fCollision :: {-# UNPACK #-} !Vector2
  } deriving (Show)

-- | Zero force accumulator
zeroForces :: ForceAccumulator
zeroForces = ForceAccumulator (0,0) (0,0) (0,0) (0,0)

-- | Combine all forces for a particle efficiently
computeAllForces :: World -> OptimizedGrid -> V.Vector Particle -> Int -> ForceAccumulator
computeAllForces world grid particles idx =
  let p = particles V.! idx
      neighborIndices = getNeighborIndices grid particles idx (h world)
      neighbors = map (particles V.!) neighborIndices
      forces = foldl' (computeForceContribution world p) zeroForces neighbors
  in forces

-- | Compute force contribution from a single neighbor
computeForceContribution :: World -> Particle -> ForceAccumulator -> Particle -> ForceAccumulator
computeForceContribution world p acc n =
  let rVec = vecSub (position p) (position n)
      r = vecMagnitude rVec
      
  in if r > 0 && r < h world
     then let -- Pressure force
              pressureGrad = spikyGrad rVec (h world)
              pressureMag = if density n > 0 
                           then (pressure p / (density p * density p)) + 
                                (pressure n / (density n * density n))
                           else 0
              massSquared = (mass world) * (mass world)
              pressureForce = vecScale (massSquared * pressureMag) pressureGrad
              
              -- Viscosity force
              vDiff = vecSub (velocity n) (velocity p)
              viscLap = viscosityLaplacian r (h world)
              viscForce = if density n > 0 
                         then vecScale (viscosity world * mass world * viscLap / density n) vDiff
                         else (0,0)
              
              -- Collision force
              minDist = (h world) * 0.3
              overlap = minDist - r
              collisionForce = if overlap > 0
                              then let force = 2000.0 * overlap / r
                                       direction = vecNormalize rVec
                                   in vecScale force direction
                              else (0,0)
              
              -- Surface tension (simplified)
              surfaceForce = if density n > 0 && r < h world * 0.8
                            then let gradContrib = colorFieldGrad rVec (h world)
                                     massOverDensity = mass world / density n
                                     force = vecScale (surfaceTension world * massOverDensity * 0.1) gradContrib
                                 in force
                            else (0,0)
              
          in ForceAccumulator 
               { fPressure = vecAdd (fPressure acc) pressureForce
               , fViscosity = vecAdd (fViscosity acc) viscForce
               , fSurface = vecAdd (fSurface acc) surfaceForce
               , fCollision = vecAdd (fCollision acc) collisionForce
               }
     else acc

-- | Parallel position and velocity update
updatePositionVelocityParallel :: World -> OptimizedGrid -> V.Vector Particle -> V.Vector Particle
updatePositionVelocityParallel world grid particles =
  let indices = [0 .. V.length particles - 1]
      chunkSize = max 1 (V.length particles `div` 4)
      chunks = chunksOf chunkSize indices
      
      processChunk chunk = map (updateSingleParticle world grid particles) chunk
      
      results = parMap rseq processChunk chunks
      flatResults = concat results
      
  in V.fromList flatResults

-- | Update single particle with optimized force computation
updateSingleParticle :: World -> OptimizedGrid -> V.Vector Particle -> Int -> Particle
updateSingleParticle world grid particles idx =
  let p = particles V.! idx
      forces = computeAllForces world grid particles idx
      fCursor = cursorForce world p
      fBall = ballForce world p
      fGravity = vecScale (mass world) (gravity world)
      
      -- Combine all forces
      fTotal = foldl' vecAdd (0,0) 
        [ fPressure forces
        , fViscosity forces  
        , fSurface forces
        , fCollision forces
        , fCursor
        , fBall
        , fGravity
        ]
      
      dt = 0.016
      acceleration = vecScale (1 / mass world) fTotal
      newVel = vecAdd (velocity p) (vecScale dt acceleration)
      newPos = vecAdd (position p) (vecScale dt newVel)
      dampedVel = vecScale 0.98 newVel
      
      -- Apply boundary conditions (scene-specific or level-specific)
      (boundedPos, finalVel) = applyBoundaryConditions world newPos dampedVel
      
  in p { position = boundedPos, velocity = finalVel }




-- | Utility function to split list into chunks
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Optimized kernels with reduced computation
wPoly6 :: Float -> Float -> Float
wPoly6 r h
  | r > h = 0
  | otherwise = let q = h^2 - r^2 in 315 / (64 * pi * h^9) * q^3

spikyGrad :: Vector2 -> Float -> Vector2
spikyGrad (dx, dy) h
  | r > h || r == 0 = (0, 0)
  | otherwise = let factor = -45 / (pi * h^6) * (h - r)^2 / r
                in (factor * dx, factor * dy)
  where r = sqrt (dx^2 + dy^2)

viscosityLaplacian :: Float -> Float -> Float
viscosityLaplacian r h
  | r > h = 0
  | otherwise = 45 / (pi * h^6) * (h - r)

colorFieldGrad :: Vector2 -> Float -> Vector2
colorFieldGrad (dx, dy) h
  | r > h || r == 0 = (0, 0)
  | otherwise = let factor = -945 / (32 * pi * h^9) * (h^2 - r^2)^2 / r
                in (factor * dx, factor * dy)
  where r = sqrt (dx^2 + dy^2)

-- | Fast vector operations
vecAdd :: Vector2 -> Vector2 -> Vector2
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)
{-# INLINE vecAdd #-}

vecSub :: Vector2 -> Vector2 -> Vector2
vecSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)
{-# INLINE vecSub #-}

vecScale :: Float -> Vector2 -> Vector2
vecScale s (x,y) = (s*x, s*y)
{-# INLINE vecScale #-}

vecMagnitude :: Vector2 -> Float
vecMagnitude (x,y) = sqrt (x^2 + y^2)
{-# INLINE vecMagnitude #-}

vecNormalize :: Vector2 -> Vector2
vecNormalize v 
  | mag == 0 = (0, 0)
  | otherwise = vecScale (1/mag) v
  where mag = vecMagnitude v
{-# INLINE vecNormalize #-}

-- | Euclidean distance between two particles
distance :: Particle -> Particle -> Float
distance p1 p2 = sqrt $ (fst (position p1) - fst (position p2))^2 + 
                   (snd (position p1) - snd (position p2))^2
{-# INLINE distance #-}

-- | SPH density computation
computeDensity :: Particle -> [Particle] -> Float -> Float -> Float
computeDensity p neighbors massVal hVal = 
  massVal * sum [wPoly6 (distance p neighbor) hVal | neighbor <- neighbors]

-- | Interactive cursor force
cursorForce :: World -> Particle -> Vector2
cursorForce world p
  | not (mouseDown world) = (0, 0)
  | otherwise =
      let (mx, my) = mousePos world
          (px, py) = position p
          dx = px - mx
          dy = py - my
          r = sqrt (dx*dx + dy*dy)
          cursorRadius = 50
          maxForce = 800
      in if r < cursorRadius && r > 0
         then let factor = maxForce * (1 - r/cursorRadius) / r
              in (dx * factor, dy * factor)
         else (0, 0)

-- -- | Ball force for Ball scene
-- ballForce :: World -> Particle -> Vector2
-- ballForce world p
--   | scene world /= Ball = (0, 0)
--   | otherwise =
--       let center = (0, 0)
--           radius = 50
--           (px, py) = position p
--           dx = px - fst center
--           dy = py - snd center
--           r = sqrt (dx*dx + dy*dy)
--       in if r < radius && r > 0
--          then let factor = 1000 * (1 - r/radius) / r
--               in (dx * factor, dy * factor)
--          else (0, 0)

-- | Apply boundary conditions based on scene (enhanced with level collision)
applyBoundaryConditions :: World -> Vector2 -> Vector2 -> (Vector2, Vector2)
applyBoundaryConditions world newPos newVel =
  case scene world of
    PuzzleLevel -> 
      -- Use level collision detection
      let particle = Particle newPos newVel 0 0  -- Temporary particle for collision check
          (finalPos, finalVel) = checkLevelCollision world particle (newPos, newVel)
      in (finalPos, finalVel)
    
    Square ->
      let (x, y) = newPos
          (vx, vy) = newVel
          (boundedX, newVx) 
            | x < -180 = (-180, abs vx * 0.5)
            | x > 180  = (180, -abs vx * 0.5)
            | otherwise = (x, vx)
          (boundedY, newVy) 
            | y < -180 = (-180, abs vy * 0.5)
            | y > 180  = (180, -abs vy * 0.5)
            | otherwise = (y, vy)
      in ((boundedX, boundedY), (newVx, newVy))
    
    Hourglass ->
      let (x, y) = newPos
          (vx, vy) = newVel
          (boundedY, newVy) 
            | y < -180 = (-180, abs vy * 0.5)
            | y > 180  = (180, -abs vy * 0.5)
            | otherwise = (y, vy)
          
          -- Define triangle walls
          leftWallX = 
            if y > 0
            then -180 + (180 - y) * (176 / 180)
            else -4 - (abs y) * (176 / 180)
          
          rightWallX = 
            if y > 0
            then 180 - (180 - y) * (176 / 180)
            else 4 + (abs y) * (176 / 180)
          
          -- Handle collision with triangle walls
          (boundedX, newVx) 
            | x < leftWallX = (leftWallX, abs vx * 0.5)
            | x > rightWallX = (rightWallX, -abs vx * 0.5)
            | otherwise = (x, vx)
          
      in ((boundedX, boundedY), (newVx, newVy))

    Ball ->
      let (x, y) = newPos
          (vx, vy) = newVel
          (boundedX, newVx) 
            | x < -180 = (-180, abs vx * 0.5)
            | x > 180  = (180, -abs vx * 0.5)
            | otherwise = (x, vx)
          (boundedY, newVy) 
            | y < -180 = (-180, abs vy * 0.5)
            | y > 180  = (180, -abs vy * 0.5)
            | otherwise = (y, vy)
      in ((boundedX, boundedY), (newVx, newVy))



    Windmill ->
      let (x, y) = newPos
          (vx, vy) = newVel
          (boundedX, newVx) 
            | x < -180 = (-180, abs vx * 0.5)
            | x > 180  = (180, -abs vx * 0.5)
            | otherwise = (x, vx)
          (boundedY, newVy) 
            | y < -180 = (-180, abs vy * 0.5)
            | y > 180  = (180, -abs vy * 0.5)
            | otherwise = (y, vy)
          -- Обработка столкновения с мельницей
          (millX, millY, millVx, millVy) = checkMillCollision world (boundedX, boundedY) (newVx, newVy)
      in ((millX, millY), (millVx, millVy))






checkTriangleCollisions :: Vector2 -> Vector2 -> (Float, Float, Float, Float)
checkTriangleCollisions (x, y) (vx, vy) =
  let 
      leftUpper = (x - (-90)) / (0 - (-90)) > (y - 90) / (0 - 90) 
      rightUpper = (x - 90) / (0 - 90) > (y - 90) / (0 - 90)   

      leftLower = (x - (-90)) / (0 - (-90)) < (y - (-90)) / (0 - (-90))
      rightLower = (x - 90) / (0 - 90) < (y - (-90)) / (0 - (-90))     
  in if (y > 0 && (leftUpper || rightUpper)) || (y < 0 && (leftLower || rightLower))
     then let
            -- Инвертируем скорость с затуханием
            newVx = -vx * 0.5
            newVy = -vy * 0.5
            -- Сдвигаем частицу обратно
            offset = 1.0
            newX = if leftUpper || leftLower then x + offset else if rightUpper || rightLower then x - offset else x
            newY = if y > 0 then y - offset else y + offset
          in (newX, newVx, newY, newVy)
     else (x, vx, y, vy)

-- Добавим функцию ballForce
ballForce :: World -> Particle -> Vector2
ballForce world p
  | scene world /= Ball = (0, 0)
  | otherwise =
      let center = (0, 0)
          radius = 50
          (px, py) = position p
          dx = px - fst center
          dy = py - snd center
          r = sqrt (dx*dx + dy*dy)
      in if r < radius && r > 0
         then let factor = 1000 * (1 - r/radius) / r
              in (dx * factor, dy * factor)
         else (0, 0)



checkMillCollision :: World -> Vector2 -> Vector2 -> (Float, Float, Float, Float)
checkMillCollision world (x, y) (vx, vy) =
  let angle = windmillAngle world
      -- Преобразуем координаты частицы в систему координат мельницы
      relX = x * cos angle + y * sin angle
      relY = -x * sin angle + y * cos angle
      
      -- Проверяем столкновение с горизонтальной лопастью
      collideHoriz = abs relY < 5 && abs relX < 60
      -- Проверяем столкновение с вертикальной лопастью
      collideVert = abs relX < 5 && abs relY < 60
      
      -- Если есть столкновение
      collide = collideHoriz || collideVert
  in if collide
     then
        -- Вычисляем нормаль в системе координат мельницы
        let norm = if collideHoriz then (0, signum relY) else (signum relX, 0)
            -- Преобразуем нормаль обратно в мировые координаты
            worldNormX = normX * cos angle - normY * sin angle
            worldNormY = normX * sin angle + normY * cos angle
            (normX, normY) = norm
            -- Отражение скорости
            dotProduct = vx * worldNormX + vy * worldNormY
            newVx = vx - 1.8 * dotProduct * worldNormX
            newVy = vy - 1.8 * dotProduct * worldNormY
            -- Сдвиг частицы во избежание залипания
            offset = 1.0
            newX = x + offset * worldNormX
            newY = y + offset * worldNormY
        in (newX, newVx, newY, newVy)
     else (x, vx, y, vy)
