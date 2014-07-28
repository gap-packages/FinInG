q := 5;
ps := ParabolicQuadric(4,q);
ps := EllipticQuadric(5,q);
pts := Set(AsList(Points(ps)));;
lines := Set(AsList(Lines(ps)));;
inc := \*;
group := CollineationGroup(ps);
gp := GeneralisedPolygonByElements(pts,lines,inc);
gp := GeneralisedPolygonByElements(pts,lines,inc,group);
gp := GeneralisedPolygonByElements2(pts,lines,inc,group,\^);
graph := IncidenceGraphOfGeneralisedPolygon(gp);;
vn := VertexNames(graph);;

p := Random(Points(gp));
test1 := Set(List(Lines(p)));;
test2 := Set(List(vn{Adjacency(graph,Position(pts,Unwrap(p)))},x->Wrap(gp,2,x)));;


#### our best friend T_2(0)
conic := Set(Points(ParabolicQuadric(2,q)));
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(conic,x->x^em);
group := CollineationGroup(pg);
stab := FiningSetwiseStabiliser(group,O);
points1 := Set(Filtered(Points(pg),x->not x in hyp));;
phi := PolarityOfProjectiveSpace(ParabolicQuadric(2,q));
tangents := List(conic,x->x^phi);
lines := List(tangents,x->x^em);
planes := List(lines,x->Filtered(Planes(x),y->not y in hyp));
points2 := Union(planes);
points3 := [hyp];
linesa := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));
linesb := Set(O);
pts := Union(points1,points2,points3);
lns := Union(linesa,linesb);
inc := \*;
gp := GeneralisedPolygonByElements2(pts,lns,inc,stab,\^);
gp := GeneralisedPolygonByElements(pts,lns,inc,stab);

