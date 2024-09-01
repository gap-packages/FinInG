gap> START_TEST("Forms: uderlyingvs.tst");
gap> ps := ProjectiveSpace(4,16);
ProjectiveSpace(4, 16)
gap> vs := UnderlyingVectorSpace(ps);
( GF(2^4)^5 )
gap> ps := PG(1,2^8);
ProjectiveSpace(1, 256)
gap> vs := UnderlyingVectorSpace(ps);
( GF(2^8)^2 )
gap> STOP_TEST("uderlyingvs.tst", 10000 );
