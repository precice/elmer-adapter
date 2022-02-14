# Example Problem: Flow over heated plate

Refer to [the preCICE tutorials](https://github.com/precice/tutorials/tree/master/flow-over-heated-plate) for a detailed explanation of the case. Note: only explicit coupling is supported.

## Setup

Compile the coupler solver with `elmerf90 -o Coupler_Solver.so Coupler_Solver.F90 -lprecice`. Refer to the partitioned heat conduction case for a more detailed description.

## Elmer-Elmer coupling

Explicit Elmer-Elmer coupling works out of the box with the files provided here.

## Elmer-OpenFOAM

For Elmer-OpenFOAM coupling (with OpenFOAM as fluid participant) copy the fluid-openfoam directory from the [tutorials reposity](https://github.com/precice/tutorials/tree/master/flow-over-heated-plate) and use the `Coupler_solver_elmer-openfoam.F90` and `precice-config_elmer-openfoam.xml` (just remove the *_elmer-openfoam* in the filenames and compile the solver). These adjustments are required due to different naming of the variables in the Elmer, that currently require an hard-coded adjustment in the `Coupler_solver.F90`.
