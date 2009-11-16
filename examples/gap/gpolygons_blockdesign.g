# gpolygons_blockdesign.g
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t*id );;
egq := EGQByqClan( clan, f );
HasElationGroup( egq );
design := BlockDesignOfGeneralisedPolygon( egq );;
aut := AutGroupBlockDesign( design );
NrBlockDesignPoints( design );
NrBlockDesignBlocks( design );
DisplayCompositionSeries(aut);
quit;
