gap> START_TEST("fining: tst_fining11.tst");
gap> #Test enumerators of symplectic polar spaces in odd char. Few cases not to exagerate. May take a few minutes.
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
gap> ps := SymplecticSpace(5,3);
W(5, 3)
gap> els := Points(ps);
<points of W(5, 3)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <points of W(5, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 364 ] ]
gap> els := Lines(ps);
<lines of W(5, 3)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <lines of W(5, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 3640 ] ]
gap> els := Planes(ps);
<planes of W(5, 3)>
gap> enum := Enumerator(els);
EnumeratorOfSubspacesOfClassicalPolarSpace( <planes of W(5, 3)> )
gap> Collected(List(AsList(els),x->enum[Position(enum,x)]=x));
[ [ true, 1120 ] ]
gap> STOP_TEST("tst_fining11.tst", 10000 );
