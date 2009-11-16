# gpolygons_collineations.g
LoadPackage("Grape");
Print("Collineations of projective planes...\n");
blocks := [ 
   [ 1, 2, 3, 4, 5 ], [ 1, 6, 7, 8, 9 ], [ 1, 10, 11, 12, 13 ],
   [ 1, 14, 15, 16, 17 ], [ 1, 18, 19, 20, 21 ], [ 2, 6, 10, 14, 18 ], 
   [ 2, 7, 11, 15, 19 ], [ 2, 8, 12, 16, 20 ], [ 2, 9, 13, 17, 21 ], 
   [ 3, 6, 11, 16, 21 ], [ 3, 7, 10, 17, 20 ], [ 3, 8, 13, 14, 19 ], 
   [ 3, 9, 12, 15, 18 ], [ 4, 6, 12, 17, 19 ], [ 4, 7, 13, 16, 18 ], 
   [ 4, 8, 10, 15, 21 ], [ 4, 9, 11, 14, 20 ], [ 5, 6, 13, 15, 20 ], 
   [ 5, 7, 12, 14, 21 ], [ 5, 8, 11, 17, 18 ], [ 5, 9, 10, 16, 19 ] ];;
pp := ProjectivePlaneByBlocks( blocks );
coll := CollineationGroup( pp );
DisplayCompositionSeries( coll );
Display( CollineationAction(coll) );

Print("Collineations of generalised hexagons...\n");
hex := SplitCayleyHexagon( 5 );
coll := CollineationGroup( hex );
CollineationAction(coll) = OnProjSubspaces;

Print("Collineations of elation generalised quadrangles...\n");
g := ElementaryAbelianGroup(27);
flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]), 
            Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];; 
egq := EGQByKantorFamily(g, flist1, flist2);
elations := ElationGroup( egq );
CollineationAction(elations) = OnKantorFamily;
HasCollineationGroup( egq );