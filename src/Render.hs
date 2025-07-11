module Render where

import Types
import Graphics.Gloss
import Data.List (nub)
import Physics (distance)

-- Blend particles as additive metaballs with concentric alpha layers
-- More fluid-like rendering with transparency
particleToPicture :: Float -> Particle -> Picture
particleToPicture hVal p = translate x y $ pictures layers
  where
    (x, y) = position p
    densityRatio = min 1.0 (density p / 2000)
    -- Softer color gradient
    colorBase = (0.0, 0.5 + densityRatio*0.5, 1.0)
    -- Larger, softer particles
    layers = 
      [ color (makeColor r g b (0.3 * a)) $ circleSolid (hVal * size)
      | (size, a) <- [(0.8, 1.0), (1.0, 0.6), (1.2, 0.3)]
      , let (r, g, b) = colorBase
      ]

-- Thicker surface lines for fluid appearance
surfaceLines :: Float -> [Particle] -> [Picture]
surfaceLines hVal ps = map linePic segments
  where
    neighbors p = [ n | n <- ps, n /= p, distance p n < hVal * 1.0 ]  -- Increased range
    segments = nub [ (position p, position n) | p <- ps, n <- neighbors p ]
    linePic (p1, p2) = color (makeColor 0.0 0.7 1.0 0.4) $ thickLine 6 p1 p2  -- Thicker lines

drawCursor :: World -> Picture
drawCursor world
  | mouseDown world = 
      let (x, y) = mousePos world
          radius = 50
      in translate x y $ color (makeColor 1 0 0 0.3) $ circleSolid radius
  | otherwise = Blank

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
renderWorld world = pictures [drawContainer, surface, blobs, cursor]
  where
    ps   = particles world
    hVal = h world
    surface = pictures $ surfaceLines hVal ps
    blobs   = pictures $ map (particleToPicture hVal) ps
    cursor  = drawCursor world

-- Draw container boundaries
drawContainer :: Picture
drawContainer = color white $ lineLoop [(-180,-180),(-180,180),(180,180),(180,-180)]