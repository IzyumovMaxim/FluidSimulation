module Render where

import Types
import Graphics.Gloss
import Data.List (nub)
import Physics (distance)

-- Blend particles as additive metaballs with concentric alpha layers
particleToPicture :: Float -> Particle -> Picture
particleToPicture hVal p = translate x y $ pictures layers
  where
    (x, y) = position p
    densityRatio = min 1.0 (density p / 2000)
    baseAlpha = 0.2 + densityRatio * 0.4
    radii = [hVal * 0.8, hVal * 0.5, hVal * 0.3]
    alphas = [baseAlpha, baseAlpha*0.6, baseAlpha*0.3]
    layers = [ color (makeColor 0.0 (0.5 + densityRatio*0.5) 1.0 a) $ circleSolid r
             | (r, a) <- zip radii alphas ]

-- Create soft surface mesh by drawing thick line segments between close particles
surfaceLines :: Float -> [Particle] -> [Picture]
surfaceLines hVal ps = map linePic segments
  where
    neighbors p = [ n | n <- ps, n /= p, distance p n < hVal * 0.7 ]
    segments = nub [ (position p, position n) | p <- ps, n <- neighbors p ]
    linePic (p1, p2) = color (makeColor 0.0 0.7 1.0 0.3) $ thickLine 4 p1 p2

-- Helper: draw thick rectangle between two points
thickLine :: Float -> Point -> Point -> Picture
thickLine w (x1,y1) (x2,y2) = translate mx my $ rotate ang $ rectangleSolid len w
  where
    dx = x2 - x1; dy = y2 - y1
    len = sqrt (dx*dx + dy*dy)
    ang = atan2 dy dx * 180 / pi
    mx = (x1 + x2) / 2
    my = (y1 + y2) / 2

-- Full render combining blobs and surface
renderWorld :: World -> Picture
renderWorld world = pictures [drawContainer, surface, blobs]
  where
    ps   = particles world
    hVal = h world
    surface = pictures $ surfaceLines hVal ps
    blobs   = pictures $ map (particleToPicture hVal) ps

-- Draw container boundaries
drawContainer :: Picture
drawContainer = color white $ lineLoop [(-180,-180),(-180,180),(180,180),(180,-180)]
