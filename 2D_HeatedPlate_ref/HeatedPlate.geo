//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {0, 2, 0, 1.0};
//+
Point(3) = {2, 2, 0, 1.0};
//+
Point(4) = {2, 0, 0, 1.0};
//+
Line(1) = {1, 4};
//+
Line(2) = {4, 3};
//+
Line(3) = {3, 2};
//+
Line(4) = {2, 1};
//+
Line Loop(1) = {4, 1, 2, 3};
//+
Plane Surface(1) = {1};
//+
Physical Line("HotBottom") = {1};
//+
Physical Line("ColdTop") = {3};
//+
Physical Line("Isolation") = {4, 2};
//+
Transfinite Surface {1};
//+
Transfinite Surface {1} = {3, 2, 1, 4};
//+
Transfinite Line {4, 3, 2, 1} = 11 Using Progression 1;
//+
Recombine Surface {1};
//+
Physical Surface("Plate") = {1};
