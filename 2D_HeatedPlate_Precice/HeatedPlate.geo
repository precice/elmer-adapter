//+
Point(1) = {0, 0, 0, 1.0};
//+
Point(2) = {0, 1, 0, 1.0};
//+
Point(3) = {0, 2, 0, 1.0};
//+
Point(4) = {2, 2, 0, 1.0};
//+
Point(5) = {2, 1, 0, 1.0};
//+
Point(6) = {2, 0, 0, 1.0};
//+
Line(1) = {1, 6};
//+
Line(2) = {6, 5};
//+
Line(3) = {5, 2};
//+
Line(4) = {2, 1};
//+
Line(5) = {2, 3};
//+
Line(6) = {3, 4};
//+
Line(7) = {4, 5};
//+
Line Loop(1) = {3, 5, 6, 7};
//+
Plane Surface(1) = {1};
//+
Line Loop(2) = {4, 1, 2, 3};
//+
Plane Surface(2) = {2};
//+
Physical Line("HotBottom") = {1};
//+
Physical Line("ColdTop") = {6};
//+
Physical Line("Interface") = {3};
//+
Physical Line("Isolating_lower") = {4, 2};
//+
Physical Line("Isolating_upper") = {5, 7};
//+
Physical Surface("Lower_Plate") = {2};
//+
Physical Surface("Upper_Plate") = {1};
//+
Transfinite Surface {2} = {1, 2, 5, 6};
//+
Transfinite Surface {1} = {2, 3, 4, 5};
//+
Recombine Surface {1};
//+
Recombine Surface {2};

