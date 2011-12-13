## EGQ from blt set
clan := LinearqClan(3);
bltset := BLTSetByqClan( clan);
geo := AmbientGeometry( bltset[1] );
Display( geo );
egq := EGQByBLTSet( bltset );
quit;
