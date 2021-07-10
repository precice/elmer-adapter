//+
Point(1) = {1, 0, 0, 1.0};

//+
Point(2) = {2, 0, 0, 1.0};
//+
Point(3) = {1, 1, 0, 1.0};
//+
Point(4) = {2, 1, 0, 1.0};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 4};
//+
Line(3) = {4, 3};
//+
Line(4) = {3, 1};
//+
Line Loop(1) = {4, 1, 2, 3};
//+
Plane Surface(1) = {1};
//+
Physical Line("Plate_Boundary") = {3, 2, 1};
//+
Physical Line("Coupling_Interface") = {4};
//+
Physical Surface("Plate") = {1};
//+
Transfinite Line {4, 3, 2, 1} = 10 Using Progression 1;
//+
Transfinite Surface {1};
