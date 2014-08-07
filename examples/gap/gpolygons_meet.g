# meet
mat := [ [ 1, 1, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 1, 1, 0, 0 ],
         [ 1, 0, 1, 0, 0, 0, 1 ], [ 0, 1, 1, 1, 0, 0, 0 ],
         [ 0, 1, 0, 0, 1, 0, 1 ], [ 0, 0, 0, 1, 0, 1, 1 ],
         [ 0, 0, 1, 0, 1, 1, 0 ] ];
gp := GeneralisedPolygonByIncidenceMatrix(mat);
l := Random(Lines(gp));
m := Random(Lines(gp));
Meet(l,m);
ps := ParabolicQuadric(4,3);
gp := GeneralisedPolygonByElements(Set(Points(ps)),Set(Lines(ps)),\*);
l := Random(Lines(gp));
m := Random(Lines(gp));
Meet(l,m);
quit;

