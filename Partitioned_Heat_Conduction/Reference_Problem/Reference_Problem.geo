//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {0, 1, 0, 1.0};
//+
Point(3) = {1, 1, 0, 1.0};
//+
Point(4) = {1, 0, 0, 1.0};
//+
Line(1) = {1, 4};
//+
Line(2) = {4, 3};
//+
Line(3) = {3, 2};
//+
Line(4) = {2, 1};
//+
Line Loop(1) = {3, 4, 1, 2};
//+
Plane Surface(1) = {1};
//+
Physical Line("Dirichlet_Boundary") = {4, 3, 2, 1};
//+
Physical Surface("Heated_Plate") = {1};

//+
Transfinite Surface {1} = {1, 2, 3, 4};
//+
Transfinite Line {4, 2} = 11 Using Progression 1;
//+
Transfinite Line {1, 3} = 11 Using Progression 1;
//+
Recombine Surface {1};
