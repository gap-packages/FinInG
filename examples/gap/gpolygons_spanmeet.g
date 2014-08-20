#span and meet for classical generalised hexagons.
ps := SymplecticSpace(5,8);
gh := SplitCayleyHexagon(ps);
vec := [ Z(2)^0, Z(2^3)^6, Z(2^3)^5, Z(2^3)^6, Z(2)^0, Z(2^3) ];
p := VectorSpaceToElement(gh,vec);
vec := [ Z(2)^0, Z(2^3)^2, Z(2^3), Z(2^3)^3, Z(2^3)^5, Z(2^3)^5 ];
q := VectorSpaceToElement(gh,vec);
span := Span(p,q);
ElementToElement(gh,span);
vec := [ [ Z(2)^0, 0*Z(2), Z(2^3)^6, Z(2)^0, 0*Z(2), Z(2^3) ], 
  [ 0*Z(2), Z(2)^0, Z(2^3)^6, Z(2^3)^4, Z(2^3)^4, 0*Z(2) ] ];
l := VectorSpaceToElement(gh,vec);
vec := [ [ Z(2)^0, 0*Z(2), Z(2)^0, Z(2^3), 0*Z(2), Z(2^3) ], 
  [ 0*Z(2), Z(2)^0, Z(2)^0, Z(2^3)^2, Z(2^3)^4, Z(2^3)^4 ] ];
m := VectorSpaceToElement(gh,vec);
Meet(l,m);
DistanceBetweenElements(l,m);
quit;

