# FluidSimulation
Fluid simulation on Haskell, you can adjust parameters of fluid and interact with it via cursor (need to click and hold). We use Smoothed particle hydrodynamics (SPH) for physics simulation.

![Simulation](footage/scene_record.mov)

**Also Swampy crocodile is avaliable as a game mode**
![Photo](footage/swampy_gameplay.png)

In this version (stage 1) there is:
- basic 2D visualisation and modelling;
- one scene with some shape containing the fluid;
- easily configurable model (via coefficients and kernel functions).

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

## File structure
```
fluid-simulation/
├── src/
│   ├── assets/
    └── i.png         -- Swampy image🐊
│   ├── Physics.hs    -- SPH calculations
│   ├── Types.hs      -- Data types
│   ├── Level.hs      -- Game level with fluid-based puzzle
│   ├── Render.hs     -- Gloss visualization
│   └── Main.hs       -- Entry point
├── app.cabal         -- Build config
└── stack.yaml        -- Stack config
```
