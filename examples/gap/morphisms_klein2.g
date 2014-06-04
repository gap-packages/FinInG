#morphisms_klein.g
quadric := HyperbolicQuadric(5,3);                                         
k := KleinCorrespondence( quadric );
pg := ProjectiveSpace(3, 3);
l := Random( Lines(pg) );
l^k;
id := IdentityMat(6,GF(13));
form := QuadraticFormByMatrix(id,GF(13));
quadric := PolarSpace(form);
k := KleinCorrespondence( quadric );
pg := AmbientGeometry(Source(k));
l := Random(Lines(pg));
l^k;
quit;
