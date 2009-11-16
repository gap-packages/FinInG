# spreads of W(5,3)
id := IdentityMat(7,GF(3));;
form := QuadraticFormByMatrix(id,GF(3));
ps := PolarSpace(form);
psl32 := PSL(3,2);
reps:=[[1,1,1,0,0,0,0],[-1,1,1,0,0,0,0],
[1,-1,1,0,0,0,0],[1,1,-1,0,0,0,0]]*Z(3)^0;
ovoid := Union(List(reps,x -> Orbit(psl32,x,Permuted)));;
ovoid := List(ovoid,x -> VectorSpaceToElement(ps,x));;
planes := AsList(Planes(ps));;
ForAll(planes,p -> Number(ovoid,x -> x in p) = 1);
g := IsometryGroup(ps);
points := AsList(Points(ps));;
hom := ActionHomomorphism(g,points,OnPoints);
omega := HomeEnumerator(UnderlyingExternalSet(hom));;
imgs := Filtered([1..Size(omega)],x -> omega[x] in ovoid);;
stab := Stabilizer(Image(hom),imgs,OnSets);
stabovoid := PreImage(hom,stab);
DisplayCompositionSeries(stabovoid);
OrbitLengths(stabovoid,ovoid,OnPoints);
IsTransitive(stabovoid,ovoid,OnPoints);
quit;
