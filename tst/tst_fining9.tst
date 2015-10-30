gap> START_TEST("fining: tst_fining9.tst");
gap> #Test enumerators of hyperbolic quadrics in odd/even char. Few cases not to exagerate. May take a few minutes.
gap> ps := HyperbolicQuadric(3,9);
Q+(3, 9)
gap> els := Points(ps);
<points of Q+(3, 9)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q+(3, 9)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 100 ] ]
gap> els := Lines(ps);
<lines of Q+(3, 9)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q+(3, 9)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 20 ] ]
gap> ps := HyperbolicQuadric(5,2);
Q+(5, 2)
gap> els := Points(ps);
<points of Q+(5, 2)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q+(5, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 35 ] ]
gap> els := Lines(ps);
<lines of Q+(5, 2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q+(5, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 105 ] ]
gap> els := Planes(ps);
<planes of Q+(5, 2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <planes of Q+(5, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 30 ] ]
gap> STOP_TEST("tst_fining9.tst", 10000 );
