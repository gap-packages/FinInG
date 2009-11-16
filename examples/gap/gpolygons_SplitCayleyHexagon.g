## gpolygons_SplitCayleyHexagon.g
hexagon := SplitCayleyHexagon( 3 );
points := Points( hexagon );
lines := AsList( Lines(hexagon) );;
lines[1];
AmbientSpace( hexagon );
coll := CollineationGroup( hexagon );
DisplayCompositionSeries( coll );
quit;

