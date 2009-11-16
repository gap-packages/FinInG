#geometry of absolute points wrt polarity
mat := IdentityMat(4,GF(16));
phi := HermitianPolarityOfProjectiveSpace(mat,GF(16));
geom := GeometryOfAbsolutePoints(phi);
mat := [[1,0,0,0],[0,0,1,1],[0,1,1,0],[0,1,0,0]]*Z(32)^0;
phi := PolarityOfProjectiveSpace(mat,GF(32));
geom := GeometryOfAbsolutePoints(phi);
quit;
