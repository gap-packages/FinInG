gap> START_TEST("Forms: basefield.tst");
gap> ps := ProjectiveSpace(3,81);
ProjectiveSpace(3, 81)
gap> BaseField(ps);
GF(3^4)
gap> ps := PG(1,2^16);
ProjectiveSpace(1, 65536)
gap> BaseField(ps);
GF(2^16)
gap> ps := PG(9,3^5);
ProjectiveSpace(9, 243)
gap> BaseField(ps);
GF(3^5)
gap> PS := PG(2,7);
ProjectiveSpace(2, 7)
gap> BaseField(ps);
GF(3^5)
gap> STOP_TEST("basefield.tst", 10000 );
