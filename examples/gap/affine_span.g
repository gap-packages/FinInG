## affine_join.g
ag := AffineSpace(4,5);
p := AffineSubspace(ag, [1,0,0,0] * One(GF(5)) );
r := AffineSubspace(ag, [0,1,0,0] * One(GF(5)) );
l := Span(p, r);
l^_;
Display(l);
quit;
