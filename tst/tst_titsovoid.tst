gap> START_TEST("fining: tst_titsovoid.tst");
gap> #Tits ovoid, taken from one of the examples wit less output
gap> q := 8;
8
gap> pg := PG(3,q);
ProjectiveSpace(3, 8)
gap> f := GF(q);
GF(2^3)
gap> vecs := Union(List(f,x->List(f,y->[One(f),x*y+x^6+y^4,x,y])));;
gap> Add(vecs,[0,1,0,0]*Z(q)^0);
gap> ovoid := List(vecs,x->VectorSpaceToElement(pg,x));;
gap> numbers := List(Planes(pg),x->Number(ovoid,y->y in x));;
gap> Collected(numbers);
[ [ 1, 65 ], [ 9, 520 ] ]
gap> STOP_TEST("tst_titsovoid.tst", 10000 );
