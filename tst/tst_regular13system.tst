gap> START_TEST("fining: tst_regular13system.tst");
gap> #regular 13 system
gap> q := 3;
3
gap> gh := SplitCayleyHexagon(q);
H(3)
gap> ps := AmbientPolarSpace(gh);
Q(6, 3): -x_1*x_5-x_2*x_6-x_3*x_7+x_4^2=0
gap> coll := CollineationGroup(gh);
#I  for Split Cayley Hexagon
#I  Computing nice monomorphism...
#I  Found permutation domain...
G_2(3)
gap> orbits := FiningOrbits(coll,Planes(ps));
#I  Computing collineation group of canonical polar space...
67%..100%..[ <closed orbit, 756 points>, <closed orbit, 364 points> ]
gap> Length(orbits);
2
gap> S := First(orbits,x->Length(x)=(q^6-1)/(q-1));
<closed orbit, 364 points>
gap> pts := AsList(Points(ps));;
gap> Collected(List(pts,x->Number(S,y->x in y)));
[ [ 13, 364 ] ]
gap> ps := ParabolicQuadric(6,q);
Q(6, 3)
gap> gh := SplitCayleyHexagon(ps);
H(3) in Q(6, 3)
gap> coll := CollineationGroup(gh);
#I  for Split Cayley Hexagon
#I  Computing nice monomorphism...
#I  Found permutation domain...
<projective collineation group with 5 generators>
gap> orbits := FiningOrbits(coll,Planes(ps));
32%..100%..[ <closed orbit, 364 points>, <closed orbit, 756 points> ]
gap> Length(orbits);
2
gap> S := First(orbits,x->Length(x)=(q^6-1)/(q-1));
<closed orbit, 364 points>
gap> pts := AsList(Points(ps));;
gap> Collected(List(pts,x->Number(S,y->x in y)));
[ [ 13, 364 ] ]
gap> STOP_TEST("tst_regular13system.tst", 10000 );
