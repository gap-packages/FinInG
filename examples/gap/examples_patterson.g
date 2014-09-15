#Patterson ovoid
id := IdentityMat(7, GF(3));;
form := QuadraticFormByMatrix(id, GF(3));
ps := PolarSpace( form );
#The construction of the ovoid (a la Shult):
psl32 := PSL(3,2);
reps:=[[1,1,1,0,0,0,0], [-1,1,1,0,0,0,0],
[1,-1,1,0,0,0,0], [1,1,-1,0,0,0,0]]*Z(3)^0;
ovoid := Union( List(reps, x-> Orbit(psl32, x, Permuted)) );;
ovoid := List(ovoid, x -> VectorSpaceToElement(ps, x));;
#We check that this is indeed an ovoid...
planes := AsList(Planes( ps ));;
ForAll(planes, p -> Number(ovoid, x -> x * p) = 1);
#The stabiliser is interesting since it yields the embedding of Sp(6,2) in PO(7,3). To efficiently compute the set-wise stabiliser, we refer to the induced permutation representation.
g := IsometryGroup( ps );
stabovoid := FiningSetwiseStabiliser(g, ovoid);
DisplayCompositionSeries(stabovoid);
OrbitLengths(stabovoid, ovoid);
IsTransitive(stabovoid, ovoid);
quit;
