#morphisms_klein.g
k := KleinCorrespondence( 9 );
Intertwiner(k);
pg := ProjectiveSpace(3, 9);
AmbientGeometry(Range(k));
l := Random( Lines(pg) );
l^k;
quit;
