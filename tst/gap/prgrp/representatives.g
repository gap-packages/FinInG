#Representative, MatrixOfCollineation, MatrixOfCorrelation, BaseField, FieldAutomorphism, ProjectiveSpaceIsomorphism, Order
mat:=
[[Z(2^3)^6,Z(2^3),Z(2^3)^3,Z(2^3)^3],[Z(2^3)^6,Z(2)^0,Z(2^3)^2,Z(2^3)^3],
[0*Z(2),Z(2^3)^4,Z(2^3),Z(2^3)],[Z(2^3)^6,Z(2^3)^5,Z(2^3)^3,Z(2^3)^5 ]];
frob := FrobeniusAutomorphism(GF(8));
phi := CollineationOfProjectiveSpace(mat,frob,GF(8));
Representative(phi);
MatrixOfCollineation(phi);
BaseField(phi);
FieldAutomorphism(phi);
Order(phi);
mat := [[1,0,0],[3,0,2],[0,5,4]]*Z(7^5);
field := GF(7^5);
frob := FrobeniusAutomorphism(field);
pg := PG(2,field);
delta := StandardDualityOfProjectiveSpace(pg);
psi := CorrelationOfProjectiveSpace(mat,frob^3,field,delta);
Representative(psi);
MatrixOfCorrelation(psi);
BaseField(psi);
FieldAutomorphism(psi);
ProjectiveSpaceIsomorphism(psi);
Order(psi);
quit;
