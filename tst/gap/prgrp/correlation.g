#CorrelationOfProjectiveSpace
mat := [[1,0,0],[3,0,2],[0,5,4]]*Z(7^5);
field := GF(7^5);
frob := FrobeniusAutomorphism(field);
pg := PG(2,field);
delta := StandardDualityOfProjectiveSpace(pg);
CorrelationOfProjectiveSpace(mat,field);
CorrelationOfProjectiveSpace(mat,frob,field);
CorrelationOfProjectiveSpace(mat,frob^2,field);
CorrelationOfProjectiveSpace(mat,field,delta);
CorrelationOfProjectiveSpace(mat,frob^3,field,delta);
CorrelationOfProjectiveSpace(mat,frob^4,field,delta);
Correlation(pg,mat,frob,delta);
quit;

