#ProjectiveSemilinearMap
mat := [[Z(2^8)^31,Z(2^8)^182,Z(2^8)^49],[Z(2^8)^224,Z(2^8)^25,Z(2^8)^45],
[Z(2^8)^128,Z(2^8)^165,Z(2^8)^217]];
frob := FrobeniusAutomorphism(GF(2^8));
phi := ProjectiveSemilinearMap(mat,frob^2,GF(2^8));
xi := ProjectiveSemilinearMap(mat,frob^0,GF(2^8));
quit;

