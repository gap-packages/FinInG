#shadow of element of cps.
ps := ParabolicQuadric(4,7);
p := Random(Points(ps));
shadow := ShadowOfElement(ps,p,3);
quit;
shadow := ShadowOfElement(ps,p,2);
ps := HermitianPolarSpace(5,4);
plane := Random(Planes(ps));
shadow1 := ShadowOfElement(ps,plane,1);
shadow2 := ShadowOfElement(ps,plane,2);
id := IdentityMat(10,GF(3));
form := BilinearFormByMatrix(id,GF(3));
ps := PolarSpace(form);
Rank(ps);
ps;
plane := Random(Planes(ps));
shadow := ShadowOfElement(ps,plane,4);
List(shadow);
id := IdentityMat(8,GF(7));
form := BilinearFormByMatrix(id,GF(7));
ps := PolarSpace(form);
Rank(ps);
ps;
plane := Random(Planes(ps));
time;
shadow := ShadowOfElement(ps,plane,4);
List(shadow);
quit;

