module Physics where

import Types
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- | Spatial grid for efficient neighbor finding
-- Maps grid cell coordinates to lists of particles in that cell
type Grid = M.Map (Int, Int) [Particle]

-- | Build spatial grid for efficient neighbor queries
-- Divides space into cells slightly larger than smoothing radius
-- Each particle is assigned to one cell based on its position
buildGrid :: Float -> [Particle] -> Grid
buildGrid hVal ps = M.fromListWith (++) $ map assignCell ps
  where
    -- Cell size is 1.2 * smoothing radius to ensure all neighbors are found
    cellSize = hVal * 1.2
    assignCell p = 
      let (x, y) = position p
          i = floor (x / cellSize)
          j = floor (y / cellSize)
      in ((i, j), [p])

-- | Get neighbors using spatial grid optimization
-- Only checks particles in the current cell and 8 adjacent cells
getNeighborsGrid :: Grid -> Particle -> Float -> [Particle]
getNeighborsGrid grid p hVal = 
  let (x, y) = position p
      cellSize = hVal * 1.2
      i0 = floor (x / cellSize)
      j0 = floor (y / cellSize)
      cells = [(i0+dx, j0+dy) | dx <- [-1,0,1], dy <- [-1,0,1]]
      candidates = concatMap (\k -> fromMaybe [] (M.lookup k grid)) cells
  in filter (\n -> distance p n < hVal) candidates

-- | Compute density and pressure for a particle using SPH method
-- Density is computed using Poly6 kernel, pressure using equation of state
computeDensityPressure :: World -> Grid -> Particle -> Particle
computeDensityPressure world grid p = 
  let neighbors = getNeighborsGrid grid p (h world)
      rho = max 1.0 (computeDensity p neighbors (mass world) (h world))
      pVal = stiffness world * (rho - rho0 world)
  in p { density = rho, pressure = pVal }

-- | Poly6 kernel function for density computation
-- W(r,h) = (315/64πh⁹)(h² - r²)³ for r ≤ h, 0 otherwise
wPoly6 :: Float -> Float -> Float
wPoly6 r h
  | r > h     = 0
  | otherwise = 315 / (64 * pi * h^9) * (h^2 - r^2)^3

-- | Spiky kernel gradient for pressure forces
-- ∇W(r,h) = (-45/πh⁶)(h - r)² * (r/|r|) for r ≤ h, 0 otherwise
spikyGrad :: Vector2 -> Float -> Vector2
spikyGrad (dx, dy) h
  | r > h || r == 0 = (0, 0)
  | otherwise = (factor * dx / r, factor * dy / r)
  where 
    r = sqrt (dx^2 + dy^2)
    factor = -45 / (pi * h^6) * (h - r)^2

-- | Viscosity kernel Laplacian for viscosity forces
-- ∇²W(r,h) = (45/πh⁶)(h - r) for r ≤ h, 0 otherwise
viscosityLaplacian :: Float -> Float -> Float
viscosityLaplacian r h
  | r > h = 0
  | otherwise = 45 / (pi * h^6) * (h - r)

-- | Vector arithmetic operations
vecAdd :: Vector2 -> Vector2 -> Vector2
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

vecSub :: Vector2 -> Vector2 -> Vector2
vecSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

vecScale :: Float -> Vector2 -> Vector2
vecScale s (x,y) = (s*x, s*y)

vecMagnitude :: Vector2 -> Float
vecMagnitude (x,y) = sqrt (x^2 + y^2)

-- | Euclidean distance between two particles
distance :: Particle -> Particle -> Float
distance p1 p2 = sqrt $ (fst (position p1) - fst (position p2))^2 + 
                   (snd (position p1) - snd (position p2))^2

-- | SPH density computation using Poly6 kernel
-- ρ(r) = Σ m_j * W(|r - r_j|, h)
computeDensity :: Particle -> [Particle] -> Float -> Float -> Float
computeDensity p neighbors massVal hVal = 
  massVal * sum [wPoly6 (distance p neighbor) hVal | neighbor <- neighbors]

-- | Interactive cursor force for user interaction
-- Creates a repulsive force field around the mouse cursor
cursorForce :: World -> Particle -> Vector2
cursorForce world p
  | not (mouseDown world) = (0, 0)
  | otherwise =
      let (mx, my) = mousePos world
          (px, py) = position p
          dx = px - mx
          dy = py - my
          r = sqrt (dx*dx + dy*dy)
          cursorRadius = 50  -- Area of influence
          maxForce = 800     -- Strength of interaction
          
      in if r < cursorRadius && r > 0
         then let factor = maxForce * (1 - r/cursorRadius) / r
              in (dx * factor, dy * factor)
         else (0, 0)

-- | Main physics update: compute forces and integrate motion
-- This is the core of the SPH simulation, implementing the momentum equation
updatePositionVelocity :: World -> Grid -> Particle -> Particle
updatePositionVelocity world grid p = 
  let neighbors = getNeighborsGrid grid p (h world)
      
      -- PRESSURE FORCE COMPUTATION
      -- F_pressure = -∇P = -Σ m_j * (P_i + P_j)/(2ρ_j) * ∇W(r_ij, h)
      fPressure = foldl' addVec (0,0) $ 
        [ let rVec = vecSub (position p) (position n)
              r = vecMagnitude rVec
              pressureGrad = spikyGrad rVec (h world)
              pressureMag = if density n > 0 then (pressure p + pressure n) / (2 * density n) else 0
          in vecScale (mass world * pressureMag) pressureGrad
        | n <- neighbors
        , n /= p
        ]
      
      -- VISCOSITY FORCE COMPUTATION  
      -- F_viscosity = μ * Σ m_j * (v_j - v_i)/ρ_j * ∇²W(r_ij, h)
      fViscosity = foldl' addVec (0,0) $ 
        [ let vDiff = vecSub (velocity n) (velocity p)
              r = distance p n
              viscLap = viscosityLaplacian r (h world)
              viscForce = if density n > 0 then viscLap / density n else 0
          in vecScale (viscosity world * mass world * viscForce) vDiff
        | n <- neighbors
        , n /= p
        ]
      
      -- USER INTERACTION FORCE
      fCursor = cursorForce world p
      
      -- TOTAL FORCE COMBINATION
      -- F_total = F_pressure + F_viscosity + F_external + F_gravity
      fTotal = vecAdd (vecAdd (vecAdd fPressure fViscosity) fCursor)
                     (vecScale (mass world) (gravity world))
      
      dt = 0.03 -- Time step (seconds)
      acceleration = vecScale (1 / mass world) fTotal
      newVel = vecAdd (velocity p) (vecScale dt acceleration)
      newPos = vecAdd (position p) (vecScale dt newVel)
      
      -- Apply damping
      dampedVel = vecScale 0.99 newVel
      
      -- BOUNDARY CONDITIONS (reflective walls)
      -- Confine simulation to [-180, 180] x [-180, 180] box
      (x,y) = newPos
      (vx, vy) = dampedVel

      -- X-axis boundaries with velocity reflection and energy loss
      (boundedX, newVx) 
        | x < -180 = (-180, abs vx * 0.5) -- Left wall: reflect and dampen
        | x > 180  = (180, -abs vx * 0.5) -- Right wall: reflect and dampen
        | otherwise = (x, vx) -- No collision

      -- Y-axis boundaries with velocity reflection and energy loss 
      (boundedY, newVy) 
        | y < -180 = (-180, abs vy * 0.5) -- Bottom wall: reflect and dampen
        | y > 180  = (180, -abs vy * 0.5) -- Top wall: reflect and dampen
        | otherwise = (y, vy) -- No collision
      
      boundedPos = (boundedX, boundedY)
      finalVel = (newVx, newVy)
      
  in p { position = boundedPos, velocity = finalVel }
  where addVec = vecAdd
