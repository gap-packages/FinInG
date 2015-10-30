##################################################
##
## In this example, we construct the unique ovoid
## of Q(6,3), first discovered by Patterson, but for
## which was given a nice construction by E. Shult.
##
## We begin with a the "sums of squares" quadratic
## form over GF(3)...
##
##################################################

id := IdentityMat(7, GF(3));;
form := QuadraticFormByMatrix(id, GF(3));
ps := PolarSpace( form );

## The construction of the ovoid:

psl32 := PSL(3,2);
reps:=[[1,1,1,0,0,0,0],
[-1,1,1,0,0,0,0],
[1,-1,1,0,0,0,0],
[1,1,-1,0,0,0,0]]*Z(3)^0;

ovoid := Union( List(reps, x-> Orbit(psl32, x, Permuted)) );;
ovoid := List(ovoid, x -> VectorSpaceToElement(ps, x));;

## We check that this is indeed an ovoid

planes := AsList( Planes( ps ) );;
ForAll(planes, p -> Number(ovoid, x -> x in p) = 1);


## Now we find the stabiliser

g := CollineationGroup(ps);
stab := SetwiseStabilizer(g, OnProjSubspaces, ovoid)!.setstab;
DisplayCompositionSeries(stab);
OrbitLengths(stab, AsList(Points(ps)));