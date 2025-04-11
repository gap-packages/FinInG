gap> START_TEST("Forms: ambientspace.tst");
gap> pg := PG(2,2);
ProjectiveSpace(2, 2)
gap> AmbientSpace(pg);
ProjectiveSpace(2, 2)
gap> pg := ProjectiveSpace(4,243);
ProjectiveSpace(4, 243)
gap> AmbientSpace(pg);
ProjectiveSpace(4, 243)
gap> AmbientSpace(pg)=pg;
true
gap> STOP_TEST("ambientspace.tst", 10000 );
