#base field of a projective space
f := GF(5);
mat := IdentityMat(4,f);
form := BilinearFormByMatrix(mat,f);
pg := PG(3,5);
pts := Filtered(Points(pg),x->[x,x]^form = Zero(f));;
ps := PolarSpace(form);
Collected(List(pts,x->x in ps));
Size(Points(ps));
qform := QuadraticForm(ps);
pts2 := Filtered(Points(pg),x->x^qform = Zero(f));;
Collected(List(pts2,x->x in ps));
quit;
