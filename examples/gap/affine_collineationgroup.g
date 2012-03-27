#affine collineation group
as := AffineSpace(4,8);
g := CollineationGroup(as);
h := AffineGroup(as);
IsSubgroup(g,h);
as := AffineSpace(4,7);
g := CollineationGroup(as);
quit;

