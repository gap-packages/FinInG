#constructing an element of a polar space
ps := SymplecticSpace(3,4);
v := [1,0,1,0]*Z(4)^0;
p := VectorSpaceToElement(ps,v);
mat := [[1,1,0,1],[0,0,1,0]]*Z(4)^0;
line := VectorSpaceToElement(ps,mat);
quit;
mat := [[1,1,0,0],[0,0,1,0]]*Z(4)^0;
line := VectorSpaceToElement(ps,mat);
p := VectorSpaceToElement(ps,[0,0,0,0]*Z(4)^0);
quit;
