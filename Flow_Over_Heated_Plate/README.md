# elmer-adapter
**experimental** preCICE-adapter for the open source multiphysical simulation software Elmer FEM

# Getting started

## Dependencies & Installation Instructions

* preCICE
    * Recommended: Install debian package, please refer to https://precice.org/installation-overview.html for installation
* Elmer
    * Recommended: Install debian package, please refer to http://www.elmerfem.org/blog/binaries/ 

## Use the adapter

The adapter uses a custom-made Elmer solver for coupling with preCICE. This solver is compiled using `elmerf90` and then plugged into the `case.sif` file in a minimally-invaisve fashion. 
Compile the solver by running `elmerf90 -o Coupler_Solver.so Coupler_Solver.F90 -lprecice`.

## How to couple your own code

We assume you already have your own application in Elmer and you want to couple it to another solver using preCICE. Your application includes a `case.sif` file and the required mesh files.

1. Take one of the existing example cases and copy the `Coupler_Solver.F90` from there into your project folder.
2. Use the provided instructions to compile the adapter.
3. Use one of the provided `case.sif` files from the example as a template and add the respective commands for calling the adapter in your `case.sif` file.
4. Create an appropriate `precice-config.xml` for the coupling.
5. Run your case in Elmer via `ElmerSolver case.sif`. You should now see that Elmer waits for the other participant. When you start the other participant your coupled simulation will be executed.

## Current restrictions & future work

Currently, implicit coupling is not supported by the adapter. Parallelization and other advanced features were not explicitly developed or tested.