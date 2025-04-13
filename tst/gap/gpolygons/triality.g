#Generalized hexgons to test trialities
hexagon := SplitCayleyHexagon( 3 );
AmbientPolarSpace(hexagon);
ps := ParabolicQuadric(6,3);
hexagon := SplitCayleyHexagon( ps );
AmbientPolarSpace(hexagon);
hexagon := SplitCayleyHexagon( 4 );
AmbientPolarSpace(hexagon);
ps := ParabolicQuadric(6,4);
hexagon := SplitCayleyHexagon( ps );
AmbientPolarSpace(hexagon);
ps := SymplecticSpace(5,8);
gh := SplitCayleyHexagon(ps);
vec := [ Z(2)^0, Z(2^3)^6, Z(2^3)^5, Z(2^3)^6, Z(2)^0, Z(2^3) ];
p := VectorSpaceToElement(gh,vec);
vec := [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^3), Z(2^3)^6, Z(2^3)^6 ];
q := VectorSpaceToElement(gh,vec);
span := Span(p,q);
ElementToElement(gh,span);
vec := [ [ Z(2)^0, 0*Z(2), Z(2^3)^4, Z(2^3)^5, Z(2^3)^6, Z(2^3)^2 ],
  [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^3)^2, Z(2^3)^6, Z(2^3)^5 ] ];
l := VectorSpaceToElement(gh,vec);
vec := [ [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2^3), Z(2^3)^5 ],
  [ 0*Z(2), Z(2)^0, Z(2^3)^5, Z(2^3)^4, Z(2^3)^5, Z(2)^0 ] ];
m := VectorSpaceToElement(gh,vec);
Meet(l,m);
DistanceBetweenElements(l,m);
hexagon := TwistedTrialityHexagon(2^3);
AmbientPolarSpace(hexagon);
ps := HyperbolicQuadric(7,2^3);
hexagon := TwistedTrialityHexagon(ps);
AmbientPolarSpace(hexagon);
quit;
