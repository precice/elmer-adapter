//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {6, 0, 0, 1.0};
//+
Point(3) = {0, 4, 0, 1.0};
//+
Point(4) = {6, 4, 0, 1.0};
//+
Line(2) = {2, 4};
//+
Line(3) = {4, 3};
//+
Line(4) = {3, 1};
//+
Point(5) = {3, 0, 0, 1.0};
//+
Point(6) = {3.1, 0, 0, 1.0};
//+
Point(7) = {3, 1, 0, 1.0};
//+
Point(8) = {3.1, 1, 0, 1.0};
//+
Line(5) = {7, 5};
//+
Line(6) = {7, 8};
//+
Line(7) = {8, 6};
//+
Line(8) = {1, 5};
//+
Line(9) = {5, 6};
//+
Line(10) = {6, 2};

//+
Line Loop(1) = {8, -5, 6, 7, 10, 2, 3, 4};
//+
Plane Surface(1) = {1};
//+
Line Loop(2) = {5, 9, -7, -6};
//+
Plane Surface(2) = {2};
//+
Physical Surface("Fluid") = {1};
//+
Physical Surface("Flap") = {2};
//+
Physical Line("Inlet") = {4};
//+
Physical Line("Outlet") = {2};
//+
Physical Line("Pipe_Boundary") = {8, 10, 3};
//+
Physical Line("Flap_Boundary") = {5, 6, 7};
//+
Physical Line("Flap_Bottom") = {9};
