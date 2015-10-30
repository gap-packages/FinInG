gap> START_TEST("fining: tst_fining7.tst");
gap> #Test enumerators of parabolic quadrics in even char. Few cases not to exagerate. May take a few minutes.
gap> ps := ParabolicQuadric(4,7);
Q(4, 7)
gap> els := Points(ps);
<points of Q(4, 7)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q(4, 7)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 400 ] ]
gap> els := Lines(ps);
<lines of Q(4, 7)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q(4, 7)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 400 ] ]
gap> ps := ParabolicQuadric(6,3);
Q(6, 3)
gap> els := Points(ps);
<points of Q(6, 3)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q(6, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 364 ] ]
gap> els := Lines(ps);
<lines of Q(6, 3)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q(6, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 3640 ] ]
gap> els := Planes(ps);
<planes of Q(6, 3)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <planes of Q(6, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 1120 ] ]
gap> STOP_TEST("tst_fining7.tst", 10000 );
