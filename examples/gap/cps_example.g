#an extra example to construct polar spaces, including an ovoid
id := IdentityMat(7, GF(3));;
form := QuadraticFormByMatrix(id, GF(3));
ps := PolarSpace( form );
psl32 := PSL(3,2);
reps:=[[1,1,1,0,0,0,0], [-1,1,1,0,0,0,0], [1,-1,1,0,0,0,0], [1,1,-1,0,0,0,0]]*Z(3)^0;;
ovoid := Union( List(reps, x-> Orbit(psl32, x, Permuted)) );;
ovoid := List(ovoid, x -> VectorSpaceToElement(ps, x));;
planes := AsList( Planes( ps ) );;
ForAll(planes, p -> Number(ovoid, x -> x in p) = 1);
quit;

