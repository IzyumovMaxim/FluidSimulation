module Render where

import Types
import Graphics.Gloss
import Data.List (nub)
import Physics (distance)

-- | Render particle as metaball with concentric alpha layers
particleToPicture :: Float -> Particle -> Picture
particleToPicture hVal p = translate x y $ pictures layers
  where
    (x, y) = position p
    densityRatio = min 1.0 (density p / 2000)
    colorBase = (0.0, 0.5 + densityRatio*0.5, 1.0)
    layers =
      [ color (makeColor r g b (0.3 * a)) $ circleSolid (hVal * size)
      | (size, a) <- [(0.8, 1.0), (1.0, 0.6), (1.2, 0.3)]
      , let (r, g, b) = colorBase
      ]

-- | Draw interactive cursor when mouse is pressed
drawCursor :: World -> Picture
drawCursor world
  | mouseDown world =
      let (x, y) = mousePos world
          radius = 50
      in translate x y $ color (makeColor 1 0 0 0.3) $ circleSolid radius
  | otherwise = Blank


-- | Display current simulation parameters as on-screen text
drawParams :: World -> Picture
drawParams world =
  let params = [ "Gravity: " ++ show (gravity world)
               , "Mass: " ++ show (mass world)
               , "Rest density: " ++ show (rho0 world)
               , "Stiffness: " ++ show (stiffness world)
               , "Viscosity: " ++ show (viscosity world)
               , "Surface tension: " ++ show (surfaceTension world)
               , "Smoothing radius: " ++ show (h world)
               , "Controls:"
               , " R - Reset simulation"
               , " Arrows - Gravity"
               , " T/G - Mass"
               , " Y/H - Density"
               , " U/J - Stiffness"
               , " I/K - Viscosity"
               , " P/; - Surface tension"
               , " O/L - Smoothing radius"
               ]
      yStart = 350
      xStart = -390
      lineHeight = 20
      pictures = [ translate xStart (yStart - fromIntegral (i * lineHeight)) $
                   scale 0.1 0.1 $ color white $ text param
                 | (i, param) <- zip [0..] params ]
  in Pictures pictures

-- | Main rendering function - combines all visual elements
renderWorld :: World -> Picture
renderWorld world = pictures [drawContainer, blobs, cursor, drawParams world]
  where
    ps = particles world
    hVal = h world
    blobs = pictures $ map (particleToPicture hVal) ps
    cursor = drawCursor world

-- | Draw simulation container boundaries
drawContainer :: Picture
drawContainer = color white $ lineLoop [(-180,-180),(-180,180),(180,180),(180,-180)]