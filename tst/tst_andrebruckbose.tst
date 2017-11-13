gap> START_TEST("fining: tst_andrebruckbose.tst");
gap> # Desarguesian plane in Andre Bruck Bose construction
gap> q := 3;
3
gap> pg1 := PG(1,q^2);
ProjectiveSpace(1, 9)
gap> em := NaturalEmbeddingByFieldReduction(pg1,GF(q));
<geometry morphism from <All elements of ProjectiveSpace(1, 
9)> to <All elements of ProjectiveSpace(3, 3)>>
gap> spread := List(Points(pg1),x->x^em);
[ <a line in ProjectiveSpace(3, 3)>, <a line in ProjectiveSpace(3, 3)>, 
  <a line in ProjectiveSpace(3, 3)>, <a line in ProjectiveSpace(3, 3)>, 
  <a line in ProjectiveSpace(3, 3)>, <a line in ProjectiveSpace(3, 3)>, 
  <a line in ProjectiveSpace(3, 3)>, <a line in ProjectiveSpace(3, 3)>, 
  <a line in ProjectiveSpace(3, 3)>, <a line in ProjectiveSpace(3, 3)> ]
gap> pg := PG(4,q);
ProjectiveSpace(4, 3)
gap> inf := HyperplaneByDualCoordinates(pg,[1,0,0,0,0]*Z(q)^0);
<a solid in ProjectiveSpace(4, 3)>
gap> em2 := NaturalEmbeddingBySubspace(PG(3,q),pg,inf);
<geometry morphism from <All elements of ProjectiveSpace(3, 
3)> to <All elements of ProjectiveSpace(4, 3)>>
gap> inf_pts := List(spread,x->x^em2);
[ <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)>, 
  <a line in ProjectiveSpace(4, 3)>, <a line in ProjectiveSpace(4, 3)> ]
gap> stab1 := FiningStabiliser(CollineationGroup(pg),inf);;
gap> stab2 := FiningSetwiseStabiliser(stab1,inf_pts);;
#I  Computing adjusted stabilizer chain...
gap> affine_pts := Filtered(Points(pg),x->not x in inf);;
gap> pts := Union(affine_pts,inf_pts);;
gap> affine_lines := Union(List(inf_pts,x->Filtered(Planes(x),y->not y in inf)));;
gap> lines := Union(affine_lines,[inf]);;
gap> gp := GeneralisedPolygonByElements(pts,lines,\*,stab2,OnProjSubspaces);
<projective plane order 9>
gap> coll := CollineationGroup(gp);;
gap> Order(CollineationGroup(PG(2,q^2)))=Order(coll);
true
gap> STOP_TEST("tst_andrebruckbose.tst", 10000 );
