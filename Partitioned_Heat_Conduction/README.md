# Example Problem: Partitioned heat conduction

Refer to [partitioned heat conduction tutorial](https://github.com/precice/tutorials/tree/master/partitioned-heat-conduction) for a detailed explanation of the case.

## Contents of this folder

* `precice-config.xml`: A preCICE configuration for explicit coupling.
* `precice-adapter-config-N.json`: A modified FEniCS adapter configuration file.
* Three folders (`Reference_Problem/`, `Dirichlet_Participant/` or `Neumann_Participant/` one for each case):
    * `case.sif`: This is the system input file generated by ElmerGUI (or written by hand), to be fed to `ElmerSolver` together with the mesh files to run the simulation.
    * `<Mesh_Name>.geo`: This is the Gmsh file to build geometry and generate the `.msh` file that defines mesh.
    * `<Mesh_Name>.msh`: This is the `.msh` file for ElmerSolver. It can be generated from the `.geo` file using Gmsh, but it is provided in this repository for convenience.

## Running the example

The example is provided in three different versions: A reference case, where the problem is solved monolithically and a Dirichlet and Neumann solver, which can be used for computing either side of the domain. For all three cases the same steps must be executed:

1. Go To `Reference_Problem/`, `Dirichlet_Participant/` or `Neumann_Participant/` - depending on what you want to do.
2. Generate mesh: type `ElmerGrid 14 2 <Mesh_Name>.msh`.
3. Link or copy over the compiled adapter library files (`.so`) to this folder (`Partitioned_Heat_Conduction`).
4. Run the simulation  `ElmerSolver case.sif`. For the monolithic case you only need to run a single processes. For the partitioned case you have to run two processes in independent terminals.
5. Visualize results: results are stored in the results folder (`out`) and may be visualized using Paraview.

**Hint** Results in other examples not necessarily exist in Results folder, the location can be modified in `.sif` file.

## Running the Coupled simulation for Elmer-Elmer coupling

This scenario uses two independent Elmer simulations. One for the Dirichlet participant, one for the Neumann participant. Follow the steps above for `Dirichlet_Participant` and `Neumann_Participant`.

## Running the Coupled simulation for Elmer-FEniCS coupling

**IMPORTANT** A change is required in `Coupler_Solver.F90`. See the function `CopyWriteData`. The required change is highlighted in the comments.

**RESTRICTION** This case only works, if the Elmer solver is the Dirichlet participant and the FEniCS solver is the Neumann participant.

### Preparation

Copy the `fenics` case from [the preCICE tutorials](https://github.com/precice/tutorials/tree/master/partitioned-heat-conduction) into this folder (`Partitioned_Heat_Conduction/`) (tested for https://github.com/precice/tutorials/tree/v202104.1.1). This should result in the following folder structure:

```bash
Partitioned_Heat_Conduction/
├── Coupler_Solver.F90
├── Coupler_Solver.so
├── Dirichlet_Participant
│   └── ...
├── fenics (new folder!)
│   ├── ... (everything from the tutorials)
├── Neumann_Participant
│   └── ...
├── precice-config.xml
├── README.md
└── Reference_Problem
    └── ...
```

Then, install all dependencies that are required for running the FEniCS-FEniCS partitioned heat conduction case.

**NOTE** Please replace `fenics/precice-adapter-config-N.json` with the modified `precice-adapter-config-N.json` from this folder. Comment out the error check `assert (error_total < total_error_tol)` in `fenics/errorcomputation.py`.

### Running the Elmer-FEniCS case

Follow the steps described above for running `Dirichlet_Participant/`. Run the FEniCS participant as a Neumann solver following the instructions [in the preCICE tutorials](https://github.com/precice/tutorials/tree/master/partitioned-heat-conduction).

## Information about mesh preparation

The `.geo` file defines the geometry and the mesh generator using Gmsh, it defines the 2x2 Plate and its 3 boundaries it can be open using Gmsh,just make sure that Gmsh is installed and run at the current directory. The mesh is a uniform rectangular mesh, you can adjust its size but changing the number of points on the lines using a text editor.

These steps are required if the user wants to create a `.msh` file, Gmsh software is required, open the `.geo` in Gmsh by running the command `gmsh <FileName>.geo`. You can also refer to [this tutorial](https://www.youtube.com/watch?v=O1FyiBBuN98&ab_channel=JoshTheEngineer).

### Generating Mesh Files for Elmer (required)

The mesh created by Fmsh `.msh` has to be processed by Elmer to be usable. Elmer offers two ways to create the mesh:

* using ElmerGrid: run `ElmerGrid 14 2 Reference_Problem_grid.msh -out Reference_Problem_Mesh` and the mesh files will be generated in a separate folder under same name.
* using ElmerGUI: open the `msh` file in `ElmerGUI`, define the problem, save the project and generate the `.sif` file, the mesh files will be generated in the location defined in the model/setup under MeshDB parameter. For more information please refer to the [ElmerGUI Tutorials](http://www.nic.funet.fi/pub/sci/physics/elmer/doc/ElmerTutorials.pdf).
