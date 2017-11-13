gap> START_TEST("fining: tst_enumerators.tst");
gap> #test some enumerators of polar spaces. We have to make a selection
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
gap> ps := ParabolicQuadric(6,3);
Q(6, 3)
gap> els := Points(ps);
<points of Q(6, 3)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q(6, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 364 ] ]
gap> ps := ParabolicQuadric(6,3);
Q(6, 3)
gap> els := Points(ps);
<points of Q(6, 3)>
gap> enum := Enumerator(Points(ps));
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of Q(6, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 364 ] ]
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
gap> ps := SymplecticSpace(3,9);
W(3, 9)
gap> els := Points(ps);
<points of W(3, 9)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of W(3, 9)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 820 ] ]
gap> els := Lines(ps);
<lines of W(3, 9)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of W(3, 9)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 820 ] ]
gap> STOP_TEST("tst_enumerators.tst", 10000 );
