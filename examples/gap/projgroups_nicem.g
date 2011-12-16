#nice monomorphism for projective semilinear groups
g := HomographyGroup(PG(4,8));
NiceMonomorphism(g);
Image(last);
g := CollineationGroup(PG(4,8));
NiceMonomorphism(g);
Image(last);
quit;

