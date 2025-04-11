#CollineationOfProjectiveSpace
mat:=
[[Z(2^3)^6,Z(2^3),Z(2^3)^3,Z(2^3)^3],[Z(2^3)^6,Z(2)^0,Z(2^3)^2,Z(2^3)^3],
[0*Z(2),Z(2^3)^4,Z(2^3),Z(2^3)],[Z(2^3)^6,Z(2^3)^5,Z(2^3)^3,Z(2^3)^5 ]];
frob := FrobeniusAutomorphism(GF(8));
phi := CollineationOfProjectiveSpace(mat,frob,GF(8));
psi := CollineationOfProjectiveSpace(mat,GF(8));
phi = psi;
phi := CollineationOfProjectiveSpace(PG(3,8),mat,frob);
psi := CollineationOfProjectiveSpace(PG(3,8),mat);
phi = psi;
phi := Collineation(PG(3,8),mat,frob);
psi := Collineation(PG(3,8),mat);
phi = psi;
quit;
