#morphisms_klein.g
quadric := HyperbolicQuadric(5,3);                                         
k := KleinCorrespondence( quadric );
pg := ProjectiveSpace(3, 3);
l := Random( Lines(pg) );
l^k;
quit;
