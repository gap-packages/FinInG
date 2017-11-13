# Desarguesian plane in Andre Bruck Bose construction
q := 3;
pg1 := PG(1,q^2);
em := NaturalEmbeddingByFieldReduction(pg1,GF(q));
spread := List(Points(pg1),x->x^em);
pg := PG(4,q);
inf := HyperplaneByDualCoordinates(pg,[1,0,0,0,0]*Z(q)^0);
em2 := NaturalEmbeddingBySubspace(PG(3,q),pg,inf);
inf_pts := List(spread,x->x^em2);
stab1 := FiningStabiliser(CollineationGroup(pg),inf);;
stab2 := FiningSetwiseStabiliser(stab1,inf_pts);;
affine_pts := Filtered(Points(pg),x->not x in inf);;
pts := Union(affine_pts,inf_pts);;
affine_lines := Union(List(inf_pts,x->Filtered(Planes(x),y->not y in inf)));;
lines := Union(affine_lines,[inf]);;
gp := GeneralisedPolygonByElements(pts,lines,\*,stab2,OnProjSubspaces);
coll := CollineationGroup(gp);;
Order(CollineationGroup(PG(2,q^2)))=Order(coll);
quit;
