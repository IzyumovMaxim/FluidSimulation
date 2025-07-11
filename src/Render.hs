module Render where

import Types
import Graphics.Gloss
import Data.List (nub)
import Physics (distance)

-- | Render particle as metaball with concentric alpha layers
-- Creates fluid-like appearance through additive blending and transparency
-- Each particle is rendered as multiple concentric circles with decreasing opacity
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

-- | Draw surface tension lines between nearby particles
-- Creates mesh-like appearance showing fluid connectivity
-- Helps visualize the fluid's internal structure and surface tension
surfaceLines :: Float -> [Particle] -> [Picture]
surfaceLines hVal ps = map linePic segments
  where
    neighbors p = [ n | n <- ps, n /= p, distance p n < hVal * 1.0 ]  -- Increased range
    segments = nub [ (position p, position n) | p <- ps, n <- neighbors p ]
    linePic (p1, p2) = color (makeColor 0.0 0.7 1.0 0.4) $ thickLine 6 p1 p2  -- Thicker lines

-- | Draw interactive cursor when mouse is pressed
-- Shows the area of influence for user interaction
drawCursor :: World -> Picture
drawCursor world
  | mouseDown world = 
      let (x, y) = mousePos world
          radius = 50
      in translate x y $ color (makeColor 1 0 0 0.3) $ circleSolid radius
  | otherwise = Blank

-- | Helper function: draw thick line between two points
-- Creates rectangular shape connecting two points with specified thickness
-- Used for surface tension lines and other thick line rendering
thickLine :: Float -> Point -> Point -> Picture
thickLine w (x1,y1) (x2,y2) = translate mx my $ rotate ang $ rectangleSolid len w
  where
    dx = x2 - x1; dy = y2 - y1
    len = sqrt (dx*dx + dy*dy)
    ang = atan2 dy dx * 180 / pi
    mx = (x1 + x2) / 2
    my = (y1 + y2) / 2

-- | Display current simulation parameters as on-screen text
-- Provides real-time feedback about simulation state and controls
drawParams :: World -> Picture
drawParams world = 
  let params = [ "Gravity: " ++ show (gravity world)
               , "Mass: " ++ show (mass world)
               , "Rest density: " ++ show (rho0 world)
               , "Stiffness: " ++ show (stiffness world)
               , "Viscosity: " ++ show (viscosity world)
               , "Smoothing radius: " ++ show (h world)
               , "Controls:"
               , "  R - Reset simulation"
               , "  Arrows - Gravity"
               , "  T/G - Mass"
               , "  Y/H - Density"
               , "  U/J - Stiffness"
               , "  I/K - Viscosity"
               , "  O/L - Smoothing radius"
               ]
      yStart = 350   -- Top of screen
      xStart = -390  -- Left edge
      lineHeight = 20
      pictures = [ translate xStart (yStart - fromIntegral (i * lineHeight)) $ 
                  scale 0.1 0.1 $ color white $ text param 
                | (i, param) <- zip [0..] params ]
  in Pictures pictures

-- | Main rendering function - combines all visual elements
-- Renders the complete simulation scene with proper layering
renderWorld :: World -> Picture
renderWorld world = pictures [drawContainer, surface, blobs, cursor, drawParams world]
  where
    ps   = particles world
    hVal = h world
    surface = pictures $ surfaceLines hVal ps
    blobs   = pictures $ map (particleToPicture hVal) ps
    cursor  = drawCursor world

-- | Draw simulation container boundaries
-- Shows the confined space where fluid simulation occurs
-- Matches the boundary conditions in the physics module
drawContainer :: Picture
drawContainer = color white $ lineLoop [(-180,-180),(-180,180),(180,180),(180,-180)]
