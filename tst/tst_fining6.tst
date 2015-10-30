gap> START_TEST("fining: tst_fining6.tst");
gap> #Test enumerators of parabolic quadrics in even char. Few cases not to exagerate
gap> ps := ParabolicQuadric(4,8);
Q(4, 8)
gap> els := Points(ps);
<points of Q(4, 8)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q(4, 8)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 585 ] ]
gap> els := Lines(ps);
<lines of Q(4, 8)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q(4, 8)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 585 ] ]
gap> ps := ParabolicQuadric(6,2);
Q(6, 2)
gap> els := Points(ps);
<points of Q(6, 2)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q(6, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 63 ] ]
gap> els := Lines(ps);
<lines of Q(6, 2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of Q(6, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 315 ] ]
gap> els := Planes(ps);
<planes of Q(6, 2)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <planes of Q(6, 2)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 135 ] ]
gap> STOP_TEST("tst_fining6.tst", 10000 );
