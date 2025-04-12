gap> START_TEST("Forms: subspaces.tst");
gap> pg := PG(4,53);
ProjectiveSpace(4, 53)
gap> vs := UnderlyingVectorSpace(pg);
( GF(53)^5 )
gap> subs := Subspaces(vs,2);
Subspaces( ( GF(53)^5 ), 2 )
gap> Random(subs);
<vector space of dimension 2 over GF(53)>
gap> STOP_TEST("subspaces.tst", 10000 );
