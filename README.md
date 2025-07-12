# FluidSimulation
Fluid simulation on Haskell, you can adjust parameters of fluid and interact with it via cursor (need to click and hold). We use Smoothed particle hydrodynamics (SPH) for physics simulation.

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
stack exec fluid-sim
```

## File structure
```
fluid-simulation/
├── src/
│   ├── Physics.hs    -- SPH calculations
│   ├── Types.hs      -- Data types
│   ├── Render.hs     -- Gloss visualization
│   └── Main.hs       -- Entry point
├── app.cabal         -- Build config
└── stack.yaml        -- Stack config
```
