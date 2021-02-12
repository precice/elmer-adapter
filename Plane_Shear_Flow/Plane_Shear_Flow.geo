//+
Point(1) = {-0.5, 0, 0, 1.0};
//+
Point(2) = {-0.5, 0.5, 0, 1.0};
//+
Point(3) = {0, 0.5, 0, 1.0};
//+
Point(4) = {0, 0, 0, 1.0};
//+
Point(5) = {1, 0, 0, 1.0};
//+
Point(6) = {1, 0.5, 0, 1.0};
//+
Point(7) = {0.9, -0.3, 0, 1.0};
//+
Recursive Delete {
  Point{4}; 
}
//+
Recursive Delete {
  Point{7}; 
}
//+
Point(7) = {0, 0, 0, 1.0};
//+
Point(8) = {3, 0, 0, 1.0};
//+
Point(9) = {3, 0.5, 0, 1.0};
//+
Line(1) = {1, 7};
//+
Line(2) = {7, 5};
//+
Line(3) = {5, 8};
//+
Line(4) = {8, 9};
//+
Line(5) = {9, 6};
//+
Line(6) = {6, 3};
//+
Line(7) = {3, 2};
//+
Line(8) = {2, 1};
//+
Line Loop(1) = {7, 8, 1, 2, 3, 4, 5, 6};
//+
Plane Surface(1) = {1};
//+
Physical Line("Fluid_Walls") = {7, 2, 1, 6, 5, 3};
//+
Physical Line("Inlet") = {8};
//+
Physical Line("Outlet") = {4};
//+
Physical Surface("Fluid") = {1};
//+
Physical Line("Fluid_Walls") -= {2};
//+
Physical Line("coupling_boundary") = {2};
