## gpolygons_EGQByBLTSet.g
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t * id );;
bltset := BLTSetByqClan( clan, f );
geo := AmbientGeometry( bltset[1] );
Display( geo );
egq := EGQByBLTSet( bltset );
quit;
