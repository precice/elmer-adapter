# elmer-adapter
**experimental** preCICE-adapter for the open source multiphysical simulation software Elmer FEM

# Notes for next meeting

@ https://github.com/precice/tutorials/tree/develop/partitioned-heat-conduction, within the read me file, the recatangular domain Omega = [0,2] x [0,1] how is that.
Also for the analytical solution check 37ff the solution is not in page 37

# Example Problem
This is an example problem to qualify Data communication of data communication
developped in the adapter, it is a simple 2D problem, a 2x2 square plate
heated from the bottom at 5 degrees, while its boundary is kept constant
from the top at 0 degrees, and the sides having zero heat flux.

# Usage
There is HeatedPlate.geo file this the file defining the geometry and the 
mesh generator using gmsh, it definies the 2x2 Plate and its 3 boundaries
it can be open using gmsh,just make sure that gmsh is installed and 
run at the current directory gmsh HeatedPlate.geo . The mesh is just a uniform
recatangular mesh, you can adjust its size but changing the number of points
on the lines even from the script of text editor.

Also the mesh already saved from gmsh under HeatedPlate.msh, all you have to do
is to run ElmerGrid 14 2 HeatedPlate.msh to generate mesh files necessary for
Elmer Solver, these files will be generated in sub directory with name HeatedPlate. After that just run ElmerSolver case.sif. After the solver finishes
a directory with the name Results will be created, inside it the .vtu file
can be found and visualized in paraview.

# For Compiling user defined solver with precice
elmerf90 -o Coupler_Solver Coupler_Solver.F90 /usr/lib/x86_64-linux-gnu/libprecice.so.2

# Project description
Elmer Multiphysics solver adapter for Precice coupling library.

Multiphysics simulation is a widely spread field, and it has many important application in engineering and real science; it allows scientist and engineers 
to predict certain phenomena and their effect relatively in a precise way, such to help us in desiging sophistcated product, or analysing such natural phenomena 
to understand what happens and take precaution against it. For example, weather simulation, stress analysis in the automotive and aerospace industry to predict the
system fialure. One of the powerful tools designed for that is Elmer multiphysics simulation software, it was mainly developed by CSC – IT Center for Science in Finland 
for Glacier simulation but it has other powerful area ranging from heat condution to acoustics simulation. One motivation to use it was the crystal growth simulation 
by Leibnz institute for crystal growth. But as any softwares it has limitation, and in other domains are to be used, so the soultion is to couple different solvers together
and that is where the power of precice coupling libreary comes, precice is a cupling API that allows user to couple different solvers solving same problem 
but different part of the domain, and allows to steer the simulation and exchange data between the solvers, but in order to use any solver with precice, an adapter
has to be developed for the intended solver. And that the aim of this thesis is to develop an Elmer adapter for precice coupling library. 

# Put abstract as attachment in the mail
go to english center
make paragraphs
Also Leibnz use it for crystal growth, but some limitations,use other tools as well in combination with other solver and couple it
The solution for coupling is preCICE
3 paragraphs 
-Multiphysics
-Elmer
-Precice and aim

# Table of contents
Introduction
In between
Conclusion
think the thesis from the end, what i want to contribute to scientific community, adapter and
tutorials, section with results, application, chapter of test cases, check Makis,richard thesis
2 sections on flap test case, for me flow over hot plate,crystal growth
some times put something someone what is working on.
Applications that support conclusion.
Explaining how the results work.
understand precice,elmer
could include math it depends on the results,if needed only the math required for explaining what you need
only the governing equations is enough, no need to explain weak formulation, unless like fenics tutorial
structuring the test problem

Multiphysics simulation is a widely spread field, and it has many important application in engineering and real science; it allows scientist and engineers to predict certain phenomena and their effect relatively in a precise way, such to help us in designing sophisticated product, or analysing such natural phenomena to understand what happens and take precaution against it. For example, weather simulation, stress analysis in the automotive and aerospace industry to predict thesystem failure. One of the powerful tools designed for that is Elmer multiphysics simulation software, it was mainly developed for Glacier simulation by CSC – IT Center for Science in Finland but it has other powerful areas ranging from heat conduction to acoustics simulation. One motivation to use it was the crystal growth simulation by Leibnz institute for crystal growth. But as any softwares it has limitation, and in other domains are to be used, so the solution is to couple different solvers togetherand that is where the power of preCICE coupling library comes, precice is a coupling API that allows user to couple different solvers solving same problem but different part of the domain, and allows to steer the simulation and exchange data between the solvers, but in order to use any solver with preCICE, an adapterhas to be developed for the intended solver. And that the aim of this thesis is to develop an Elmer adapter for preCICE coupling library. 