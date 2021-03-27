#Elmer Adapter
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