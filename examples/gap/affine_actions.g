#affine actions
as := AG(3,27);
p := Random(Points(as));
g := Random(CollineationGroup(as));
OnAffineSubspaces(p,g);
p^g;
l := Random(Lines(as));
OnAffineSubspaces(l,g);
l^g;
quit;
