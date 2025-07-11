module Physics where

import Types
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- Spatial grid type
type Grid = M.Map (Int, Int) [Particle]



-- Build spatial grid
buildGrid :: Float -> [Particle] -> Grid
buildGrid hVal ps = M.fromListWith (++) $ map assignCell ps
  where
    cellSize = hVal * 1.2
    assignCell p = 
      let (x, y) = position p
          i = floor (x / cellSize)
          j = floor (y / cellSize)
      in ((i, j), [p])

-- Get neighbors using grid
getNeighborsGrid :: Grid -> Particle -> Float -> [Particle]
getNeighborsGrid grid p hVal = 
  let (x, y) = position p
      cellSize = hVal * 1.2
      i0 = floor (x / cellSize)
      j0 = floor (y / cellSize)
      cells = [(i0+dx, j0+dy) | dx <- [-1,0,1], dy <- [-1,0,1]]
      candidates = concatMap (\k -> fromMaybe [] (M.lookup k grid)) cells
  in filter (\n -> distance p n < hVal) candidates

-- Update computeDensityPressure to use grid
computeDensityPressure :: World -> Grid -> Particle -> Particle
computeDensityPressure world grid p = 
  let neighbors = getNeighborsGrid grid p (h world)
      rho = max 1.0 (computeDensity p neighbors (mass world) (h world))
      pVal = stiffness world * (rho - rho0 world)
  in p { density = rho, pressure = pVal }

-- Poly6 kernel for density computation
wPoly6 :: Float -> Float -> Float
wPoly6 r h
  | r > h     = 0
  | otherwise = 315 / (64 * pi * h^9) * (h^2 - r^2)^3

-- Spiky kernel gradient for pressure forces
spikyGrad :: Vector2 -> Float -> Vector2
spikyGrad (dx, dy) h
  | r > h || r == 0 = (0, 0)
  | otherwise = (factor * dx / r, factor * dy / r)
  where 
    r = sqrt (dx^2 + dy^2)
    factor = -45 / (pi * h^6) * (h - r)^2

-- Viscosity kernel for viscosity forces
viscosityLaplacian :: Float -> Float -> Float
viscosityLaplacian r h
  | r > h = 0
  | otherwise = 45 / (pi * h^6) * (h - r)

vecAdd :: Vector2 -> Vector2 -> Vector2
vecAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)

vecSub :: Vector2 -> Vector2 -> Vector2
vecSub (x1,y1) (x2,y2) = (x1-x2, y1-y2)

vecScale :: Float -> Vector2 -> Vector2
vecScale s (x,y) = (s*x, s*y)

vecMagnitude :: Vector2 -> Float
vecMagnitude (x,y) = sqrt (x^2 + y^2)

distance :: Particle -> Particle -> Float
distance p1 p2 = sqrt $ (fst (position p1) - fst (position p2))^2 + 
                   (snd (position p1) - snd (position p2))^2

computeDensity :: Particle -> [Particle] -> Float -> Float -> Float
computeDensity p neighbors massVal hVal = 
  massVal * sum [wPoly6 (distance p neighbor) hVal | neighbor <- neighbors]

-- Get neighbors within smoothing radius
getNeighbors :: Particle -> [Particle] -> Float -> [Particle]
getNeighbors p allParticles hVal = 
  filter (\n -> distance p n < hVal) allParticles

-- Cursor interaction force
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

-- Update position and velocity
updatePositionVelocity :: World -> Grid -> Particle -> Particle
updatePositionVelocity world grid p = 
  let neighbors = getNeighborsGrid grid p (h world)
      
      -- Pressure force
      fPressure = foldl' addVec (0,0) $ 
        [ let rVec = vecSub (position p) (position n)
              r = vecMagnitude rVec
              pressureGrad = spikyGrad rVec (h world)
              pressureMag = if density n > 0 then (pressure p + pressure n) / (2 * density n) else 0
          in vecScale (mass world * pressureMag) pressureGrad
        | n <- neighbors
        , n /= p
        ]
      
      -- Viscosity force
      fViscosity = foldl' addVec (0,0) $ 
        [ let vDiff = vecSub (velocity n) (velocity p)
              r = distance p n
              viscLap = viscosityLaplacian r (h world)
              viscForce = if density n > 0 then viscLap / density n else 0
          in vecScale (viscosity world * mass world * viscForce) vDiff
        | n <- neighbors
        , n /= p
        ]
      
      -- Cursor interaction
      fCursor = cursorForce world p
      
      -- Total force
      fTotal = vecAdd (vecAdd (vecAdd fPressure fViscosity) fCursor)
                     (vecScale (mass world) (gravity world))
      
      dt = 0.03
      acceleration = vecScale (1 / mass world) fTotal
      newVel = vecAdd (velocity p) (vecScale dt acceleration)
      newPos = vecAdd (position p) (vecScale dt newVel)
      
      -- Apply damping
      dampedVel = vecScale 0.99 newVel
      
      -- Boundary conditions
      (x,y) = newPos
      (vx, vy) = dampedVel
      
      (boundedX, newVx) 
        | x < -180 = (-180, abs vx * 0.5) 
        | x > 180  = (180, -abs vx * 0.5)
        | otherwise = (x, vx)
      
      (boundedY, newVy) 
        | y < -180 = (-180, abs vy * 0.5)
        | y > 180  = (180, -abs vy * 0.5)
        | otherwise = (y, vy)
      
      boundedPos = (boundedX, boundedY)
      finalVel = (newVx, newVy)
      
  in p { position = boundedPos, velocity = finalVel }
  where addVec = vecAdd
