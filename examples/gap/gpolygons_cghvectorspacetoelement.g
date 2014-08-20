#vector space to element for hexagons
ps := ParabolicQuadric(6,9);
gh := SplitCayleyHexagon(ps);
vec := [ Z(3)^0, Z(3^2), 0*Z(3), Z(3^2), Z(3^2)^3, Z(3^2)^5, 0*Z(3) ];
p := VectorSpaceToElement(gh,vec);
vec := [ [ Z(3)^0, 0*Z(3), Z(3^2)^7, 0*Z(3), Z(3)^0, Z(3^2)^2, Z(3^2)^2 ], 
  [ 0*Z(3), Z(3)^0, 0*Z(3), Z(3)^0, 0*Z(3), Z(3^2)^3, 0*Z(3) ] ];
line := VectorSpaceToElement(gh,vec);
quit;
quit;


