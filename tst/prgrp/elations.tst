gap> START_TEST("Forms: elations.tst");
gap> ps := PG(3,9);
ProjectiveSpace(3, 9)
gap> sub := VectorSpaceToElement(ps,[[1,0,1,0],[0,1,0,1],[1,2,3,0]]*Z(3)^0);
<a plane in ProjectiveSpace(3, 9)>
gap> p1 := VectorSpaceToElement(ps,[1,0,1,2]*Z(3)^0);
<a point in ProjectiveSpace(3, 9)>
gap> p2 := VectorSpaceToElement(ps,[1,2,0,2]*Z(3)^0);
<a point in ProjectiveSpace(3, 9)>
gap> phi := ElationOfProjectiveSpace(sub,p1,p2);
< a collineation: <cmat 4x4 over GF(3,2)>, F^0>
gap> ps := PG(2,27);
ProjectiveSpace(2, 27)
gap> sub := VectorSpaceToElement(ps,[[1,0,1,],[0,1,0]]*Z(3)^0);
<a line in ProjectiveSpace(2, 27)>
gap> p := VectorSpaceToElement(ps,[1,1,1]*Z(3)^0);
<a point in ProjectiveSpace(2, 27)>
gap> g := ProjectiveElationGroup(sub,p);
<projective collineation group with 3 generators>
gap> Order(g);
27
gap> StructureDescription(g);
"C3 x C3 x C3"
gap> ps := PG(3,4);
ProjectiveSpace(3, 4)
gap> sub := Random(Hyperplanes(ps));
<a plane in ProjectiveSpace(3, 4)>
gap> g := ProjectiveElationGroup(sub);
<projective collineation group with 6 generators>
gap> Order(g);
64
gap> Transitivity(g,Difference(Points(ps),Points(sub)),OnProjSubspaces);
1
gap> StructureDescription(g); 
"C2 x C2 x C2 x C2 x C2 x C2"
gap> STOP_TEST("elations.tst", 10000 );
