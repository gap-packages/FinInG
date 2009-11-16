#absolute points wrt polarity
mat := IdentityMat(4,GF(3));
phi := PolarityOfProjectiveSpace(mat,GF(3));
points := AbsolutePoints(phi);
List(points);
quit;
