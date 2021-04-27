//+
Point(1) = {1, 0, 0, 1.0};
//+
Point(2) = {2, 0, 0, 1.0};
//+
Point(3) = {2, 1, 0, 1.0};
//+
Point(4) = {1, 1, 0, 1.0};
//+
Line(1) = {2, 2};
//+
Line(2) = {2, 1};
//+
Line(3) = {2, 3};
//+
Line(4) = {3, 4};
//+
Line(5) = {4, 1};
//+
Line Loop(1) = {4, 5, -2, 3};
//+
Plane Surface(1) = {1};
//+
Physical Line("Neumann_Coupling") = {5};
//+
Physical Line("Dirichlet_Boundary") = {4, 3, 2};
//+
Physical Surface("Heated_Plate") = {1};
//+
Transfinite Surface {1};
//+
Transfinite Surface {1} = {4, 3, 2, 1};
//+
Transfinite Line {5, 3} = 11 Using Progression 1;
//+
Transfinite Line {2, 4} = 11 Using Progression 1;
//+
Recombine Surface {1};
