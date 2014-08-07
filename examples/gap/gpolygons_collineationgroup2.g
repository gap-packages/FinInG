# collineation groups 2
mat := [ [ 1, 1, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 1, 1, 0, 0 ],
         [ 1, 0, 1, 0, 0, 0, 1 ], [ 0, 1, 1, 1, 0, 0, 0 ],
         [ 0, 1, 0, 0, 1, 0, 1 ], [ 0, 0, 0, 1, 0, 1, 1 ],
         [ 0, 0, 1, 0, 1, 1, 0 ] ];
gp := GeneralisedPolygonByIncidenceMatrix(mat);
group := CollineationGroup(gp);
gp := EGQByqClan(FisherqClan(3));
group := CollineationGroup(gp);
Order(group);
Random(group);
q := 4;
conic := ParabolicQuadric(2,q);
nucleus := NucleusOfParabolicQuadric(conic);
conic := ParabolicQuadric(2,q);
nucleus := NucleusOfParabolicQuadric(conic);
hyperoval := Union(List(Points(conic)),[nucleus]);
pg := PG(3,q);
hyp :=  HyperplaneByDualCoordinates(pg,[1,0,0,0]*Z(q)^0);
em := NaturalEmbeddingBySubspace(PG(2,q),pg,hyp);
O := List(hyperoval,x->x^em);
points := Set(Filtered(Points(pg),x->not x in hyp));;
lines := Union(List(O,x->Filtered(Lines(x),y->not y in hyp)));;
inc := \*;
gp := GeneralisedPolygonByElements(points,lines,inc);
coll := CollineationGroup(gp);
Order(coll);
Random(coll);
quit;

