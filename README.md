# elmer-adapter
**experimental** preCICE-adapter for the open source multiphysical simulation software Elmer FEM

# Getting started

## Dependencies & Installation Instructions

* preCICE
    * Recommended: Install debian package, please refer to https://precice.org/installation-overview.html for installation
* Elmer
    * Recommended: Install debian package, please refer to http://www.elmerfem.org/blog/binaries/ 

## Use the adapter

The adapter uses a custom-made Elmer solver for coupling with preCICE. This solver is compiled using `elmerf90` and then plugged into the `case.sif` file in a minimally-invaisve fashion. Examples for usage of the adapter can be found in `Partitioned_Heat_Conduction` and `Flow_Over_Heated_Plate`. For new users it is recommended to use these cases as a starting point. You can refer to the documentation of `Partitioned_Heat_Conduction` for all necessary steps.

## How to couple your own code

We assume you already have your own application in Elmer and you want to couple it to another solver using preCICE. Your application includes a `case.sif` file and the required mesh files.

1. Take one of the existing example cases and copy the `Coupler_Solver.F90` from there into your project folder.
2. Use the provided instructions to compile the adapter.
3. Use one of the provided `case.sif` files from the example as a template and add the respective commands for calling the adapter in your `case.sif` file.
4. Create an appropriate `precice-config.xml` for the coupling.
5. Run your case in Elmer via `ElmerSolver case.sif`. You should now see that Elmer waits for the other participant. When you start the other participant your coupled simulation will be executed.

## Current restrictions & future work

Currently, implicit coupling is not supported by the adapter. Parallelization and other advanced features were not explicitly developed or tested.

Partitioned heat equation is thoroughly tested for explicit coupling and gives correct results for an Elmer-Elmer coupling and for Elmer-FEniCS coupling (where Elmer is the Dirichlet participant). If Elmer is the Neumann participant in Elmer-FEniCS coupling, problems occur (probably due to the flux computation, see thesis of Hisham Saeed for details).

The example case `Perpendicular_Flap` is currently only a monolithic simulation, but a good starting point for FSI. See (the perpendicular flap tutorial)[https://github.com/precice/tutorials/tree/master/perpendicular-flap] for details.

`Coupler_Solver.F90` is currently duplicated for every example.

# Development History

The initial version of this adapter was developed by [Hisham Saeed](https://github.com/HishamSaeed) during his work on his [master's thesis](https://mediatum.ub.tum.de/604993?query=hisham&show_id=1636717&srcnodeid=604993) under supervision of [Benjamin Rodenberg](https://www.in.tum.de/i05/personen/personen/benjamin-rodenberg/).
