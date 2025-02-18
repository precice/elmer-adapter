# elmer-adapter

**experimental** preCICE-adapter for the open source multiphysical simulation software Elmer FEM

## Dependencies

* preCICE: please refer to [installation documentation](https://precice.org/installation-overview.html).
* Elmer: please refer to [Elmer documentation](http://www.elmerfem.org/blog/binaries/).

## Build the adapter

The adapter is called during runtime by Elmer. It is developed as a standalone library of Elmer features, so it has to be built before running the simulation. For building, Elmer provides a FORTRAN wrapper to make sure that code compiled by the user is compatible with `ElmerSolver`.

To build the adapter, navigate to the folder `Adapter` and run `make`. If Elmer is installed correctly, this should work out of the box.

## Use the adapter

Examples for usage of the adapter can be found in the `Partitioned_Heat_Conduction/` folder and in the preCICE tutorial [flow-over-heated-plate](https://precice.org/tutorials-flow-over-heated-plate.html). For new users it is recommended to look at the tutorial case as a starting point.

## How to couple your own code

Assuming you already have a `case.sif` file and the required mesh files, follow the steps:

1. Take one of the existing example cases and copy the `Coupler_Solver.F90` from there into your project folder.
2. Use the provided instructions to compile the adapter.
3. Use one of the provided `case.sif` files from the example as a template and add the respective commands for calling the adapter in your `case.sif` file.
4. Create an appropriate `precice-config.xml` for the coupling.
5. Run your case in Elmer via `ElmerSolver case.sif`. You should now see that Elmer waits for the other participant. When you start the other participant your coupled simulation will be executed.

## Current restrictions & future work

Currently, implicit coupling is not supported by the adapter. Parallelization and other advanced features were not explicitly developed or tested.

Partitioned heat equation is thoroughly tested for explicit coupling and gives correct results for an Elmer-Elmer coupling and for Elmer-FEniCS coupling (where Elmer is the Dirichlet participant). If Elmer is the Neumann participant in Elmer-FEniCS coupling, problems occur (probably due to the flux computation, see thesis of Hisham Saeed for details).

The example case `Perpendicular_Flap` is currently only a monolithic simulation, but a good starting point for FSI. See [the perpendicular flap tutorial](https://github.com/precice/tutorials/tree/master/perpendicular-flap) for details.

## Troubleshooting

### I cannot visualize my results with Paraview

Please make sure to use Paraview version >= 5.12. You can check your Paraview version by running `paraview --version` from the command line.

## Citing

* Elmer-preCICE: If you are using this adapter (Elmer-preCICE), please consider citing the [thesis of Hisham Saeed](https://mediatum.ub.tum.de/1636717).
* preCICE: preCICE is an academic project, developed at the [Technical University of Munich](https://www5.in.tum.de/) and at the [University of Stuttgart](https://www.ipvs.uni-stuttgart.de/). If you use preCICE, please [cite preCICE](https://precice.org/publications.html#how-to-cite-precice).
* Elmer: If you are using Elmer, please also consider citing the [Elmer repository](https://github.com/ElmerCSC/elmerfem/blob/devel/CITATION.cff).

## Development History

* The initial version of this adapter was developed by [Hisham Saeed](https://github.com/HishamSaeed) during his work on his [master's thesis](https://mediatum.ub.tum.de/1636717) under supervision of [Benjamin Rodenberg](https://www.in.tum.de/i05/personen/personen/benjamin-rodenberg/).
* The adapter and the flow over heated plate tutorial case were updated for preCICE version 3 by [Alihossein Sepahvand](https://github.com/tapegoji).
