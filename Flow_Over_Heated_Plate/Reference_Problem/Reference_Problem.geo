//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {0, -0.25, 0, 1.0};
//+
Point(3) = {1, 0, 0, 1.0};
//+
Point(4) = {1, -0.25, 0, 1.0};
//+
Line(1) = {2, 4};
//+
Line(2) = {4, 3};
//+
Line(3) = {3, 1};
//+
Line(4) = {1, 2};
//+
Line Loop(1) = {3, 4, 1, 2};
//+
Plane Surface(1) = {1};
//+
Point(5) = {1, 0.5, 0, 1.0};
//+
Point(6) = {0, 0.5, 0, 1.0};
//+
Point(7) = {-0.5, 0.5, 0, 1.0};
//+
Point(8) = {-0.5, 0, 0, 1.0};
//+
Point(9) = {3, 0, 0, 1.0};
//+
Point(10) = {3, 0.5, 0, 1.0};
//+
Line(5) = {8, 1};
//+
Line(6) = {8, 7};
//+
Line(7) = {7, 6};
//+
Line(8) = {6, 5};
//+
Line(9) = {5, 10};
//+
Line(10) = {10, 9};
//+
Line(11) = {9, 3};
//+
Line Loop(2) = {8, 9, 10, 11, 3, -5, 6, 7};
//+
Plane Surface(2) = {2};
//+
Physical Surface("Plate") = {1};
//+
Physical Surface("Fluid") = {2};
//+
Physical Line("Plate_Bottom") = {1};
//+
Physical Line("Plate_Sides") = {2, 4};
//+
Physical Line("Coupling_Interface") = {3};
//+
Physical Line("Inlet") = {6};
//+
Physical Line("Outlet") = {10};
//+
Physical Line("Pipe_Boundary") = {5, 7, 8, 9, 11};
//+
Transfinite Surface {2} = {8, 9, 10, 7};
//+
Transfinite Surface {1} = {2, 4, 3, 1};
//+
Transfinite Line {6, 7, 5, 4, 2, 10} = 5 Using Progression 1;
//+
Transfinite Line {1, 3, 8} = 10 Using Progression 1;
//+
Transfinite Line {11, 9} = 20 Using Progression 1;
