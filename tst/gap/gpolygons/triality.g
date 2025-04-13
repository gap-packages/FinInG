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
hexagon := TwistedTrialityHexagon(2^3);
AmbientPolarSpace(hexagon);
ps := HyperbolicQuadric(7,2^3);
hexagon := TwistedTrialityHexagon(ps);
AmbientPolarSpace(hexagon);
quit;
