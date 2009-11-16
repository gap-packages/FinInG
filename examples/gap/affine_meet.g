## affine_meet.g
ag := AffineSpace(4,5);
p := AffineSubspace(ag, [1,0,0,0] * One(GF(5)), 
       [[1,0,0,-1], [0,1,0,0],[0,0,1,3]] * One(GF(5)));
l := AffineSubspace(ag, [0,0,0,0] * One(GF(5)), [[1,1,0,0]] * One(GF(5)) );
x := Meet(p, l);
x^_;
Display(x);
quit;
