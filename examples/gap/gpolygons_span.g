# span
mat := [ [ 1, 1, 0, 0, 0, 1, 0 ], [ 1, 0, 0, 1, 1, 0, 0 ],
         [ 1, 0, 1, 0, 0, 0, 1 ], [ 0, 1, 1, 1, 0, 0, 0 ],
         [ 0, 1, 0, 0, 1, 0, 1 ], [ 0, 0, 0, 1, 0, 1, 1 ],
         [ 0, 0, 1, 0, 1, 1, 0 ] ];
gp := GeneralisedPolygonByIncidenceMatrix(mat);
p := Random(Points(gp));
q := Random(Points(gp));
Span(p,q);
ps := ParabolicQuadric(4,3);
gp := GeneralisedPolygonByElements(Set(Points(ps)),Set(Lines(ps)),\*);
p := Random(Points(gp));
q := Random(Points(gp));
Span(p,q);
quit;

