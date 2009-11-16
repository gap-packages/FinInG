#constructing an element of a projective space
ps := ProjectiveSpace(6,7);
v := [3,5,6,0,3,2,3,1]*Z(7)^0;
p := VectorSpaceToElement(ps,v);
Display(p);
prs := ProjectiveSpace(3,4);
pos := SymplecticSpace(3,4);
v := [1,1,0,1]*Z(4)^0;
p := VectorSpaceToElement(prs,v);
p := VectorSpaceToElement(pos,v);
mat := [[1,0,0,1],[0,1,1,0]]*Z(4)^0;
line := VectorSpaceToElement(prs,mat);
line := VectorSpaceToElement(pos,mat);
mat := [[1,0,0,1],[0,1,0,1]]*Z(4)^0;
line := VectorSpaceToElement(pos,mat);
quit;
quit;
