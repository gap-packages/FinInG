# subspaces (elementary) (2)
ps := EllipticQuadric(5,7);
vec := [1,0,0,0,0,0]*Z(7)^0;
point := VectorSpaceToElement(ps,vec);
quit;
EquationForPolarSpace(ps);
vec := [0,0,1,0,0,0]*Z(7)^0;
point := VectorSpaceToElement(ps,vec);
vec2 := [0,0,0,1,0,0]*Z(7)^0;
point2 := VectorSpaceToElement(ps,vec2);
line := Span(point,point2);
mat := [[0,0,1,0,0,0],[0,0,0,0,1,0]]*Z(7)^0;
line2 := VectorSpaceToElement(ps,mat);
meet := Meet(line,line2);
meet in ps;
point3 := ElementToElement(ps,meet);
quit;
