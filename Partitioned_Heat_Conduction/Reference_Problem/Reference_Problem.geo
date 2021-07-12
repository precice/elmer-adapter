//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {1, 0, 0, 1.0};
//+
Point(3) = {2, 0, 0, 1.0};
//+
Point(4) = {0, 1, 0, 1.0};
//+
Point(5) = {1, 1, 0, 1.0};
//+
Point(6) = {2, 1, 0, 1.0};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 3};
//+
Line(3) = {3, 6};
//+
Line(4) = {6, 5};
//+
Line(5) = {5, 4};
//+
Line(6) = {4, 1};
//+
Line Loop(1) = {5, 6, 1, 2, 3, 4};
//+
Plane Surface(1) = {1};
//+
Physical Line("Plate_Boundary") = {6, 5, 4, 3, 2, 1};
//+
Physical Surface("Plate") = {1};
//+
Transfinite Line {6, 5, 4, 3, 2, 1} = 10 Using Progression 1;
//+
Transfinite Surface {1} = {1, 3, 6, 4};
