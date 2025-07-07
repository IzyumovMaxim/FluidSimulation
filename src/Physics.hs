module Physics where

import Types
import Data.List (foldl')

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

updateParticle :: World -> Particle -> Particle
updateParticle world p = 
  let neighbors = getNeighbors p (particles world) (h world)
      rho = max 1.0 (computeDensity p neighbors (mass world) (h world))
      pVal = stiffness world * (rho - rho0 world)
      
      -- Pressure force
      fPressure = foldl' addVec (0,0) $ 
        [ let rVec = vecSub (position p) (position n)
              r = vecMagnitude rVec
              pressureGrad = spikyGrad rVec (h world)
              pressureMag = if density n > 0 then (pVal + pressure n) / (2 * density n) else 0
          in vecScale (mass world * pressureMag) pressureGrad
        | n <- neighbors
        , n /= p  -- Don't include self
        ]
      
      -- Viscosity force
      fViscosity = foldl' addVec (0,0) $ 
        [ let vDiff = vecSub (velocity n) (velocity p)
              r = distance p n
              viscLap = viscosityLaplacian r (h world)
              viscForce = if density n > 0 then viscLap / density n else 0
          in vecScale (viscosity world * mass world * viscForce) vDiff
        | n <- neighbors
        , n /= p  -- Don't include self
        ]
      
      -- Total force
      fTotal = vecAdd (vecAdd fPressure fViscosity) 
                     (vecScale (mass world) (gravity world))
      
      dt = 0.03  -- Smaller time step for stability
      acceleration = vecScale (1 / mass world) fTotal
      newVel = vecAdd (velocity p) (vecScale dt acceleration)
      newPos = vecAdd (position p) (vecScale dt newVel)
      
      -- Apply damping to velocity for stability
      dampedVel = vecScale 0.99 newVel
      
      -- Boundary conditions with collision response
      (x,y) = newPos
      (vx, vy) = dampedVel
      
      -- Bounce off walls
      (boundedX, newVx) = if x < -180 then (-180, abs vx * 0.5) 
                         else if x > 180 then (180, -abs vx * 0.5)
                         else (x, vx)
      
      (boundedY, newVy) = if y < -180 then (-180, abs vy * 0.5)
                         else if y > 180 then (180, -abs vy * 0.5)
                         else (y, vy)
      
      boundedPos = (boundedX, boundedY)
      finalVel = (newVx, newVy)
      
  in p { position = boundedPos
       , velocity = finalVel
       , density = rho
       , pressure = pVal }
  where addVec = vecAdd