#shadow of element of cps.
id := IdentityMat(8,GF(7));
form := BilinearFormByMatrix(id,GF(7));
ps := PolarSpace(form);
Rank(ps);
ps;
mat := [[1,0,0,0,3,2,0,0],[0,1,0,0,0,0,3,2],[0,0,1,0,5,3,0,0]]*Z(7)^0;
plane := VectorSpaceToElement(ps,mat);
time;
shadow := ShadowOfElement(ps,plane,4);
List(shadow);
shadow := ShadowOfElement(ps,plane,2);
quit;
