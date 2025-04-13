gap> START_TEST("Forms: triality.tst");
gap> hexagon := SplitCayleyHexagon( 3 );
H(3)
gap> AmbientPolarSpace(hexagon);
Q(6, 3): -x_1*x_5-x_2*x_6-x_3*x_7+x_4^2=0
gap> ps := ParabolicQuadric(6,3);
Q(6, 3)
gap> hexagon := SplitCayleyHexagon( ps );
H(3) in Q(6, 3)
gap> AmbientPolarSpace(hexagon);
Q(6, 3)
gap> hexagon := SplitCayleyHexagon( 4 );
H(4)
gap> AmbientPolarSpace(hexagon);
W(5, 4): x_1*y_4+x_2*y_5+x_3*y_6+x_4*y_1+x_5*y_2+x_6*y_3=0
gap> ps := ParabolicQuadric(6,4);
Q(6, 4)
gap> hexagon := SplitCayleyHexagon( ps );
H(4) in Q(6, 4)
gap> AmbientPolarSpace(hexagon);
Q(6, 4)
gap> ps := SymplecticSpace(5,8);
W(5, 8)
gap> gh := SplitCayleyHexagon(ps);
H(8) in W(5, 8)
gap> vec := [ Z(2)^0, Z(2^3)^6, Z(2^3)^5, Z(2^3)^6, Z(2)^0, Z(2^3) ];
[ Z(2)^0, Z(2^3)^6, Z(2^3)^5, Z(2^3)^6, Z(2)^0, Z(2^3) ]
gap> p := VectorSpaceToElement(gh,vec);
<a point in H(8) in W(5, 8)>
gap> vec := [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^3), Z(2^3)^6, Z(2^3)^6 ];
[ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^3), Z(2^3)^6, Z(2^3)^6 ]
gap> q := VectorSpaceToElement(gh,vec);
<a point in H(8) in W(5, 8)>
gap> span := Span(p,q);
<a line in ProjectiveSpace(5, 8)>
gap> ElementToElement(gh,span);
<a line in H(8) in W(5, 8)>
gap> vec := [ [ Z(2)^0, 0*Z(2), Z(2^3)^4, Z(2^3)^5, Z(2^3)^6, Z(2^3)^2 ],
>   [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^3)^2, Z(2^3)^6, Z(2^3)^5 ] ];
[ [ Z(2)^0, 0*Z(2), Z(2^3)^4, Z(2^3)^5, Z(2^3)^6, Z(2^3)^2 ], 
  [ 0*Z(2), Z(2)^0, 0*Z(2), Z(2^3)^2, Z(2^3)^6, Z(2^3)^5 ] ]
gap> l := VectorSpaceToElement(gh,vec);
<a line in H(8) in W(5, 8)>
gap> vec := [ [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2^3), Z(2^3)^5 ],
>   [ 0*Z(2), Z(2)^0, Z(2^3)^5, Z(2^3)^4, Z(2^3)^5, Z(2)^0 ] ];
[ [ Z(2)^0, 0*Z(2), 0*Z(2), 0*Z(2), Z(2^3), Z(2^3)^5 ], 
  [ 0*Z(2), Z(2)^0, Z(2^3)^5, Z(2^3)^4, Z(2^3)^5, Z(2)^0 ] ]
gap> m := VectorSpaceToElement(gh,vec);
<a line in H(8) in W(5, 8)>
gap> Meet(l,m);
< empty subspace >
gap> DistanceBetweenElements(l,m);
6
gap> hexagon := TwistedTrialityHexagon(2^3);
T(8, 2)
gap> AmbientPolarSpace(hexagon);
<polar space in ProjectiveSpace(
7,GF(2^3)): x_1*x_5+x_2*x_6+x_3*x_7+x_4*x_8=0 >
gap> ps := HyperbolicQuadric(7,2^3);
Q+(7, 8)
gap> hexagon := TwistedTrialityHexagon(ps);
T(8, 2) in Q+(7, 8)
gap> AmbientPolarSpace(hexagon);
Q+(7, 8)
gap> STOP_TEST("triality.tst", 10000 );
