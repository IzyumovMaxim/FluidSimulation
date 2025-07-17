# FluidSimulation
Fluid simulation on Haskell, you can adjust parameters of fluid and interact with it via cursor (need to click and hold). We use Smoothed particle hydrodynamics (SPH) for physics simulation.

![Simulation](footage/scene_record.mov)

**Also a level of Swampy the alligator game is avaliable:**
![Photo](footage/swampy_gameplay.png)


## Getting started
Clone repository
```
git clone https://github.com/IzyumovMaxim/FluidSimulation
```
Execute program
```
cd FluidSimulation
stack build
stack exec -- sph-fluid -- +RTS -N8 -s -RTS
```

## User guide
Here you can find all possible user scenarios:
<p align="left">
<img src="footage/square scene.png" width="200" />
<img src="footage/hourglass scene.png" width="200" />
<img src="footage/ball scene.png" width="180" />
<img src="footage/windmill scene.png" width="200" />
<img src="footage/swampy game.png" width="200" />
</p>


- **Game mode control keys:** 
  - 1 - Square fluid simulation
  - 2 - Hourglass fluid simulation
  - 3 - Ball fluid simulation
  - 4 - Windmill simulation
  - 5 - Puzzle Level (Where's My Water)

- **Fluid Simulation Controls:**
  - R - Reset simulation
  - Arrow keys - Adjust gravity
  - T/G - Mass up/down
  - U/J - Stiffness up/down
  - I/K - Viscosity up/down
  - P/; - Surface tension up/down
  - Q/q - Smoothing radius (affects performance)
  - W/w - Add/remove particles
  - Hold Shift for larger adjustments
  - Click and drag to interact with particles

- **Puzzle Game Controls:**
  - Click - Dig dirt blocks
  - R - Reset level
  - Goal: Collect all stars and get water to Swampy!



## Project directory structure
```
fluid-simulation/
├── src/
│   ├── assets/
│   └── i.png         -- Swampy image🐊
│   ├── Physics.hs    -- SPH calculations
│   ├── Types.hs      -- Data types
│   ├── Level.hs      -- Game level with fluid-based puzzle
│   ├── Render.hs     -- Gloss visualization
│   └── Main.hs       -- Entry point
├── app.cabal         -- Build config
└── stack.yaml        -- Stack config
```

## ChangeLog
**Stage I**
- Basic 2D visualisation and modelling;
- One scene with some shape containing the fluid;
- Easily configurable model (via coefficients and kernel functions).
  
**Stage II**
- Improved physics (added surface tension);
- Several fluid simulation scenes: sware, circle, hourglass, windmill;
- Added one level of game mode: "Where Is My Water?", as a simple fluid-based puzzle;
- Optimizations enabled:
  - Parallel computation (4 cores)
  - Vectorized operations
  - Optimized spatial grid
  - Reduced memory allocations

  
