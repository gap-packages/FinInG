# underlying object
pg := PG(2,2);
p := Random(Points(pg));
UnderlyingObject(p);
l := Random(Lines(pg));
UnderlyingObject(l);
mat := [ [ 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], 
  [ 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0 ], 
  [ 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], 
  [ 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0 ], 
  [ 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0 ], 
  [ 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0 ], 
  [ 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0 ], 
  [ 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0 ], 
  [ 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 1 ], 
  [ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0 ], 
  [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1 ] ];
gp := GeneralisedPolygonByIncidenceMatrix(mat);
p := Random(Points(gp));
UnderlyingObject(p);
l := Random(Lines(gp));
UnderlyingObject(l);
egq := EGQByBLTSet(BLTSetByqClan(LinearqClan(3)));
p := Random(Points(egq));
UnderlyingObject(p);
quit;

