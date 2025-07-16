module Physics where

import Types
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
    cellSize = hVal * 1.2  -- Moved inside the where clause
    
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
    cellSize = hVal * 1.2  -- Added where clause

-- | Parallel density and pressure computation
computeDensityPressureParallel :: World -> OptimizedGrid -> V.Vector Particle -> V.Vector Particle
computeDensityPressureParallel world grid particles = 
  let indices = [0 .. V.length particles - 1]
      chunkSize = max 1 (V.length particles `div` 4)  -- 4 chunks for parallelization
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
      
      -- Compute all forces in a single pass through neighbors
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
      fGravity = vecScale (mass world) (gravity world)
      
      -- Combine all forces
      fTotal = foldl' vecAdd (0,0) 
        [ fPressure forces
        , fViscosity forces  
        , fSurface forces
        , fCollision forces
        , fCursor
        , fGravity
        ]
      
      dt = 0.016
      acceleration = vecScale (1 / mass world) fTotal
      newVel = vecAdd (velocity p) (vecScale dt acceleration)
      newPos = vecAdd (position p) (vecScale dt newVel)
      
      dampedVel = vecScale 0.98 newVel
      (boundedPos, finalVel) = applyBoundaryConditions newPos dampedVel
      
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

-- | Apply boundary conditions
applyBoundaryConditions :: Vector2 -> Vector2 -> (Vector2, Vector2)
applyBoundaryConditions (x,y) (vx, vy) =
  let (boundedX, newVx) 
        | x < -180 = (-180, abs vx * 0.5)
        | x > 180  = (180, -abs vx * 0.5)
        | otherwise = (x, vx)

      (boundedY, newVy) 
        | y < -180 = (-180, abs vy * 0.5)
        | y > 180  = (180, -abs vy * 0.5)
        | otherwise = (y, vy)
        
  in ((boundedX, boundedY), (newVx, newVy))