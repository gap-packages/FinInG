gap> START_TEST("fining: tst_fining10.tst");
gap> #Test enumerators of hermitian polar spaces in odd/even char. Few cases not to exagerate. May take a few minutes.
gap> ps := HermitianPolarSpace(3,9);
H(3, 3^2)
gap> els := Points(ps);
<points of H(3, 3^2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of H(3, 3^2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 280 ] ]
gap> els := Lines(ps);
<lines of H(3, 3^2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of H(3, 3^2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 112 ] ]
gap> ps := HermitianPolarSpace(4,4);
H(4, 2^2)
gap> els := Points(ps);
<points of H(4, 2^2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of H(4, 2^2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 165 ] ]
gap> els := Lines(ps);
<lines of H(4, 2^2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of H(4, 2^2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 297 ] ]
gap> ps := HermitianPolarSpace(5,4);
H(5, 2^2)
gap> els := Points(ps);
<points of H(5, 2^2)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of H(5, 2^2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 693 ] ]
gap> els := Planes(ps);
<planes of H(5, 2^2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <planes of H(5, 2^2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 891 ] ]
gap> STOP_TEST("tst_fining10.tst", 10000 );
