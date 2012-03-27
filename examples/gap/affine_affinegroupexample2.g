#example of affine group (2)
G := CollineationGroup(AG(3,27));
H := CollineationGroup(PG(3,27));
g := Random(G);
g in H;
IsSubgroup(H,G);
quit;
