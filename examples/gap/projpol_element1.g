#constructing an element of a projective space
ps := ProjectiveSpace(6,7);
v := [3,5,6,0,3,2,3]*Z(7)^0;
p := VectorSpaceToElement(ps,v);
Display(p);
ps := ProjectiveSpace(3,4);
v := [1,1,0,1]*Z(4)^0;
p := VectorSpaceToElement(ps,v);
mat := [[1,0,0,1],[0,1,1,0]]*Z(4)^0;
line := VectorSpaceToElement(ps,mat);
e := VectorSpaceToElement(ps,[]);
quit;
quit;
