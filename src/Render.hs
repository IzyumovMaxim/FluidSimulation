module Render where
import Types
import Level (getBlockColor)
import Graphics.Gloss
import Data.List (nub)
import Physics (distance)
import Data.Fixed (Pico)

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

-- | Draw level geometry (blocks)
drawLevel :: World -> Picture
drawLevel world =
  case currentLevel world of
    Nothing -> Blank
    Just level ->
      let (gridW, gridH) = gridSize level
          blockSz = blockSize level
          grid = levelGrid level
          
          -- Generate all block pictures
          blockPictures = 
            [ drawBlock level (gx, gy) blockType
            | gy <- [0..gridH-1]
            , gx <- [0..gridW-1]
            , let blockType = (grid !! gy) !! gx
            , blockType /= Empty
            ]
            
      in pictures blockPictures

-- | Draw a single block
drawBlock :: Level -> GridCoord -> BlockType -> Picture
drawBlock level gridCoord blockType =
  let (worldX, worldY) = gridToWorld level gridCoord
      blockSz = blockSize level
      (r, g, b) = getBlockColor blockType
      
      blockPicture = case blockType of
        Dirt -> 
          pictures [ color (makeColor r g b 1.0) $ rectangleSolid blockSz blockSz
                   , color (makeColor (r*0.8) (g*0.8) (b*0.8) 1.0) $ rectangleWire blockSz blockSz
                   ]
        Stone ->
          pictures [ color (makeColor r g b 1.0) $ rectangleSolid blockSz blockSz
                   , color (makeColor (r*0.6) (g*0.6) (b*0.6) 1.0) $ rectangleWire blockSz blockSz
                   ]
        WaterSource ->
          pictures [ color (makeColor r g b 0.7) $ rectangleSolid blockSz blockSz
                   , color (makeColor (r*0.7) (g*0.7) (b*0.7) 1.0) $ rectangleWire blockSz blockSz
                   ]
        Goal ->
          pictures [ color (makeColor r g b 0.8) $ rectangleSolid blockSz blockSz
                   , color (makeColor (r*0.7) (g*0.7) (b*0.7) 1.0) $ rectangleWire blockSz blockSz
                   ]
        Empty -> Blank
        
  in translate worldX worldY blockPicture

-- | Draw collectible stars
drawCollectibles :: World -> Picture
drawCollectibles world =
  case currentLevel world of
    Nothing -> Blank
    Just level ->
      let stars = collectibles level
          starPictures = map drawStar stars
      in pictures starPictures

-- | Draw a single star
drawStar :: Collectible -> Picture
drawStar star
  | collected star = Blank  -- Don't draw collected stars
  | otherwise = 
      let (x, y) = starPos star
          starShape = polygon [(0, 10), (3, 3), (10, 3), (5, -2), (8, -8), (0, -5), (-8, -8), (-5, -2), (-10, 3), (-3, 3)]
      in translate x y $ pictures
           [ color (makeColor 1.0 1.0 0.0 1.0) $ starShape  -- Yellow star
           , color (makeColor 0.8 0.8 0.0 1.0) $ lineLoop [(0, 10), (3, 3), (10, 3), (5, -2), (8, -8), (0, -5), (-8, -8), (-5, -2), (-10, 3), (-3, 3)]
           ]

-- | Draw Swampy the crocodile (simplified representation)
drawSwampy :: World -> Picture -> Picture
drawSwampy world swampyPic =
  case currentLevel world of
    Nothing -> Blank
    Just level ->
      let (goalX, goalY) = fst $ goalArea level
          goalRadius = snd $ goalArea level
          
          -- Simple Swampy representation - green crocodile in a circle
          swampyPicture = pictures
            [ -- Bathtub circle
              color (makeColor 1.0 1.0 1.0 0.7) $ circleSolid (goalRadius + 15)
            , color (makeColor 0.1 0.6 0.1 1.0) $ circle (goalRadius + 15)
            , scale 0.3 0.3 swampyPic
            ]
            
      in translate goalX goalY swampyPicture

-- | Draw game UI (score, status, etc.)
drawGameUI :: World -> Picture
drawGameUI world =
  case scene world of
    PuzzleLevel ->
      let starsText = "Stars: " ++ show (collectedStars world) ++ "/3"
          stateText = case gameState world of
            Playing -> if waterInGoal world then "Water reached Swampy!" else "Dig to help water reach Swampy!"
            Won -> "LEVEL COMPLETE!"
            Lost -> "Try Again!"
          
          uiElements = 
            [ translate (-390) 350 $ scale 0.15 0.15 $ color white $ text starsText
            , translate (-390) 320 $ scale 0.12 0.12 $ color white $ text stateText
            , translate (-390) 290 $ scale 0.1 0.1 $ color white $ text "Click to dig dirt"
            , translate (-390) 270 $ scale 0.1 0.1 $ color white $ text "R - Reset level"
            , translate (-390) 250 $ scale 0.1 0.1 $ color white $ text "1 - Switch to fluid simulation menu"
            ]
            
      in pictures uiElements
    _ -> Blank

-- | Display current simulation parameters as on-screen text
drawParams :: World -> Picture
drawParams world =
  case scene world of
    PuzzleLevel -> drawGameUI world  -- Show game UI instead of physics params
    _ -> 
      let params = [ "Gravity: " ++ show (gravity world)
                   , "Mass: " ++ show (mass world)
                   , "Rest density: " ++ show (rho0 world)
                   , "Stiffness: " ++ show (stiffness world)
                   , "Viscosity: " ++ show (viscosity world)
                   , "Surface tension: " ++ show (surfaceTension world)
                   , "Smoothing radius: " ++ show (h world)
                   , "Scene: " ++ show (scene world)
                   , "Controls:"
                   , " R - Reset simulation"
                   , " Arrows - Gravity"
                   , " T/G - Mass"
                   , " Y/H - Density"
                   , " U/J - Stiffness"
                   , " I/K - Viscosity"
                   , " P/; - Surface tension"
                   , " O/L - Smoothing radius"
                   , " 1 - Square scene"
                   , " 2 - Hourglass scene"
                   , " 3 - Ball scene"
                   , " 4 - Windmill scene"
                   , " 5 - Puzzle level!"

                   ]
          yStart = 350
          xStart = -390
          lineHeight = 20
          pictures = [ translate xStart (yStart - fromIntegral (i * lineHeight)) $
                       scale 0.1 0.1 $ color white $ text param
                     | (i, param) <- zip [0..] params ]
      in Pictures pictures

-- | Main rendering function - combines all visual elements
renderWorld :: World -> Picture -> Picture
renderWorld world swampyPic = 
  case scene world of
    PuzzleLevel -> 
      pictures [ drawLevel world
               , drawCollectibles world
               , blobs
               , drawSwampy world swampyPic
               , cursor
               , drawParams world
               ]
    _ -> 
      pictures [ drawContainer world
               , blobs
               , cursor
               , drawParams world
               ]
  where
    ps = particles world
    hVal = h world
    blobs = pictures $ map (particleToPicture hVal) ps
    cursor = drawCursor world

-- | Draw simulation container boundaries based on scene
drawContainer :: World -> Picture
drawContainer world = color white $
  case scene world of
    Square -> 
      lineLoop [(-180,-180), (-180,180), (180,180), (180,-180)]      -- Whole scene square
    
    Hourglass -> 
      Pictures
        [
          lineLoop [(-180,-180), (-180,180), (180,180), (180,-180)]  -- Whole scene square
        , lineLoop [(-180,-180), (-4,0), (-180,180)]                -- Left triangle
        , lineLoop [(180,-180), (4,0), (180,180)]                   -- Right triangle
        ]

    Ball -> 
      Pictures
        [ lineLoop [(-180,-180), (-180,180), (180,180), (180,-180)]
        , translate 0 0 $ color white $ circle 45  -- White border
        ]

    Windmill -> 
      Pictures
        [ lineLoop [(-180,-180), (-180,180), (180,180), (180,-180)]  -- Границы
        , translate 0 0 $ rotate (windmillAngle world * 180/pi) $  -- Вращение
            Pictures
              [ rectangleSolid 120 10   -- Горизонтальная лопасть
              , rectangleSolid 10 120    -- Вертикальная лопасть
              ]
        ]
    PuzzleLevel -> Blank  -- Level geometry is drawn separately



