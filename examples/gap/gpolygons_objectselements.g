# objects <-> elements in generic gps.
mat := [ [ 1, 1, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 1, 1, 0, 0 ],
         [ 1, 0, 1, 0, 0, 0, 1 ], [ 0, 1, 1, 1, 0, 0, 0 ],
         [ 0, 1, 0, 0, 1, 0, 1 ], [ 0, 0, 0, 1, 0, 1, 1 ],
         [ 0, 0, 1, 0, 1, 1, 0 ] ];
gp := GeneralisedPolygonByIncidenceMatrix(mat);
p := Random(Points(gp));
UnderlyingObject(p);
l := Random(Lines(gp));
UnderlyingObject(l);
ObjectToElement(gp,1,4);
ObjectToElement(gp,2,5);
quit;
ObjectToElement(gp,2,[1,2,3]);
quit;
ObjectToElement(gp,[1,2,6]);
quit;

