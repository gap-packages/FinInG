#onprojsubspaces
ps := ProjectiveSpace(4,27);
p := VectorSpaceToElement(ps,[ Z(3^3)^22,Z(3^3)^10,Z(3^3),Z(3^3)^3,Z(3^3)^3]);
ps := ProjectiveSpace(3,27);
p := VectorSpaceToElement(ps,[ Z(3^3)^22,Z(3^3)^10,Z(3^3),Z(3^3)^3]);
Display(p);
mat := [[ Z(3^3)^25,Z(3^3)^6,Z(3^3)^7,Z(3^3)^15], 
  [Z(3^3)^9,Z(3)^0,Z(3^3)^10,Z(3^3)^18], 
  [Z(3^3)^19,0*Z(3),Z(3),Z(3^3)^12], 
  [Z(3^3)^4,Z(3^3),Z(3^3),Z(3^3)^22]];
theta := FrobeniusAutomorphism(GF(27));
phi := CollineationOfProjectiveSpace(mat,theta,GF(27));
r := OnProjSubspaces(p,phi);
Display(r);
vect := [[Z(3^3)^9,Z(3^3)^5,Z(3^3)^19,Z(3^3)^17],
  [Z(3^3)^22,Z(3^3)^22,Z(3^3)^4,Z(3^3)^17],
  [Z(3^3)^8,0*Z(3),Z(3^3)^24,Z(3^3)^21]];
s := VectorSpaceToElement(ps,vect);
r := OnProjSubspaces(s,phi);
Display(r);
quit;
