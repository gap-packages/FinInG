gap> START_TEST("fining: tst_fining8.tst");
gap> #Test enumerators of elliptic quadrics in even char. Few cases not to exagerate. May take a few minutes.
gap> ps := EllipticQuadric(3,7);
Q-(3, 7)
gap> els := Points(ps);
<points of Q-(3, 7)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q-(3, 7)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 50 ] ]
gap> ps := EllipticQuadric(5,3);
Q-(5, 3)
gap> els := Points(ps);
<points of Q-(5, 3)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q-(5, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 112 ] ]
gap> els := Lines(ps);
<lines of Q-(5, 3)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q-(5, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 280 ] ]
gap> ps := EllipticQuadric(7,2);
Q-(7, 2)
gap> els := Points(ps);
<points of Q-(7, 2)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q-(7, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 119 ] ]
gap> els := Lines(ps);
<lines of Q-(7, 2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q-(7, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 1071 ] ]
gap> els := Planes(ps);
<planes of Q-(7, 2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <planes of Q-(7, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 765 ] ]
gap> STOP_TEST("tst_fining8.tst", 10000 );
