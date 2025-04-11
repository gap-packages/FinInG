gap> START_TEST("Forms: onprojsubspaces.tst");
gap> ps := ProjectiveSpace(3,27);
ProjectiveSpace(3, 27)
gap> p := VectorSpaceToElement(ps,[Z(3^3)^22,Z(3^3)^10,Z(3^3),Z(3^3)^3]);
<a point in ProjectiveSpace(3, 27)>
gap> mat := [[ Z(3^3)^25,Z(3^3)^6,Z(3^3)^7,Z(3^3)^15],
> [Z(3^3)^9,Z(3)^0,Z(3^3)^10,Z(3^3)^18],
> [Z(3^3)^19,0*Z(3),Z(3),Z(3^3)^12], 
> [Z(3^3)^4,Z(3^3),Z(3^3),Z(3^3)^22]];
[ [ Z(3^3)^25, Z(3^3)^6, Z(3^3)^7, Z(3^3)^15 ], 
  [ Z(3^3)^9, Z(3)^0, Z(3^3)^10, Z(3^3)^18 ], 
  [ Z(3^3)^19, 0*Z(3), Z(3), Z(3^3)^12 ], 
  [ Z(3^3)^4, Z(3^3), Z(3^3), Z(3^3)^22 ] ]
gap> theta := FrobeniusAutomorphism(GF(27));
FrobeniusAutomorphism( GF(3^3) )
gap> phi := CollineationOfProjectiveSpace(mat,theta,GF(27));
< a collineation: <cmat 4x4 over GF(3,3)>, F^3>
gap> r := OnProjSubspaces(p,phi);
<a point in ProjectiveSpace(3, 27)>
gap> Unpack(UnderlyingObject(r));
[ Z(3)^0, 0*Z(3), 0*Z(3), Z(3^3)^17 ]
gap> r = p^phi;
true
gap> vect := [[Z(3^3)^9,Z(3^3)^5,Z(3^3)^19,Z(3^3)^17],
> [Z(3^3)^22,Z(3^3)^22,Z(3^3)^4,Z(3^3)^17],
> [Z(3^3)^8,0*Z(3),Z(3^3)^24,Z(3^3)^21]];
[ [ Z(3^3)^9, Z(3^3)^5, Z(3^3)^19, Z(3^3)^17 ], 
  [ Z(3^3)^22, Z(3^3)^22, Z(3^3)^4, Z(3^3)^17 ], 
  [ Z(3^3)^8, 0*Z(3), Z(3^3)^24, Z(3^3)^21 ] ]
gap> s := VectorSpaceToElement(ps,vect);
<a plane in ProjectiveSpace(3, 27)>
gap> r := OnProjSubspaces(s,phi);
<a plane in ProjectiveSpace(3, 27)>
gap> Unpack(UnderlyingObject(r));
[ [ Z(3)^0, 0*Z(3), 0*Z(3), Z(3^3)^10 ], [ 0*Z(3), Z(3)^0, 0*Z(3), Z(3^3)^16 ]
    , [ 0*Z(3), 0*Z(3), Z(3)^0, Z(3^3)^16 ] ]
gap> r = s^phi;
true
gap> STOP_TEST("onprojsubspaces.tst", 10000 );
