//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {0, 2, 0, 1.0};
//+
Recursive Delete {
  Point{2}; 
}
//+
Point(2) = {2, 0, 0, 1.0};
//+
Point(3) = {0, 1, 0, 1.0};
//+
Point(4) = {2, 1, 0, 1.0};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 2};
//+
Line(3) = {4, 2};
//+
Line(4) = {4, 3};
//+
Line(5) = {3, 1};
//+
Line Loop(1) = {4, 5, 1, -3};
//+
Plane Surface(1) = {1};
//+
Physical Line("Dirichlet_Boundary") = {5, 4, 3, 1};
//+
Physical Surface("Heated_Plate") = {1};
//+
Transfinite Line {5, 3} = 11 Using Progression 1;
//+
Transfinite Line {1, 4} = 21 Using Progression 1;
//+
Transfinite Surface {1} = {4, 2, 1, 3};
//+
Recombine Surface {1};
