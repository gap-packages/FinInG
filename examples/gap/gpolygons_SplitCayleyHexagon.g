## gpolygons_SplitCayleyHexagon.g
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
quit;

