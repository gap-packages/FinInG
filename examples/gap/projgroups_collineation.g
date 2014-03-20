#construction of a collineation.
mat:=
[[Z(2^3)^6,Z(2^3),Z(2^3)^3,Z(2^3)^3],[Z(2^3)^6,Z(2)^0,Z(2^3)^2,Z(2^3)^3],
[0*Z(2),Z(2^3)^4,Z(2^3),Z(2^3)],[Z(2^3)^6,Z(2^3)^5,Z(2^3)^3,Z(2^3)^5 ]];
frob := FrobeniusAutomorphism(GF(8));
phi := ProjectiveSemilinearMap(mat,frob^2,GF(8));
mat2 := [[Z(2^8)^31,Z(2^8)^182,Z(2^8)^49],[Z(2^8)^224,Z(2^8)^25,Z(2^8)^45], 
[Z(2^8)^128,Z(2^8)^165,Z(2^8)^217]];
psi := CollineationOfProjectiveSpace(mat2,GF(256));
quit;
