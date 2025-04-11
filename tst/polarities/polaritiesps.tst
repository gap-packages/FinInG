gap> START_TEST("Forms: polaritiesps.tst");
gap> mat := [[0,1,0],[1,0,0],[0,0,1]]*Z(169)^0;
[ [ 0*Z(13), Z(13)^0, 0*Z(13) ], [ Z(13)^0, 0*Z(13), 0*Z(13) ], 
  [ 0*Z(13), 0*Z(13), Z(13)^0 ] ]
gap> phi := PolarityOfProjectiveSpace(mat,GF(169));
<polarity of PG(2, GF(13^2)) >
gap> mat := [[Z(11)^0,0*Z(11),0*Z(11)],[0*Z(11),0*Z(11),Z(11)],
>     [0*Z(11),Z(11),0*Z(11)]];
[ [ Z(11)^0, 0*Z(11), 0*Z(11) ], [ 0*Z(11), 0*Z(11), Z(11) ], 
  [ 0*Z(11), Z(11), 0*Z(11) ] ]
gap> frob := FrobeniusAutomorphism(GF(121));
FrobeniusAutomorphism( GF(11^2) )
gap> phi := PolarityOfProjectiveSpace(mat,frob,GF(121));
<polarity of PG(2, GF(11^2)) >
gap> psi := HermitianPolarityOfProjectiveSpace(mat,GF(121));
<polarity of PG(2, GF(11^2)) >
gap> phi = psi;
true
gap> q := 9;
9
gap> mat := [[0,1/2,0],[1/2,0,0],[0,0,1]]*Z(q)^0;
[ [ 0*Z(3), Z(3), 0*Z(3) ], [ Z(3), 0*Z(3), 0*Z(3) ], 
  [ 0*Z(3), 0*Z(3), Z(3)^0 ] ]
gap> form := HermitianFormByMatrix(mat,GF(9));
< hermitian form >
gap> phi := PolarityOfProjectiveSpace(form);
<polarity of PG(2, GF(3^2)) >
gap> mat := [[0,1,0,0],[1,0,0,0],[0,0,0,1],[0,0,1,0]]*Z(16)^0;
[ [ 0*Z(2), Z(2)^0, 0*Z(2), 0*Z(2) ], [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2) ], 
  [ 0*Z(2), 0*Z(2), 0*Z(2), Z(2)^0 ], [ 0*Z(2), 0*Z(2), Z(2)^0, 0*Z(2) ] ]
gap> form := BilinearFormByMatrix(mat,GF(16));
< bilinear form >
gap> phi := PolarityOfProjectiveSpace(form);
<polarity of PG(3, GF(2^4)) >
gap> ps := HermitianPolarSpace(4,64);
H(4, 8^2)
gap> phi := PolarityOfProjectiveSpace(ps);
<polarity of PG(4, GF(2^6)) >
gap> STOP_TEST("polaritiesps.tst", 10000 );
