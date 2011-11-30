#random element of a given type of a projective space
ps := PG(4,3);
plane := Random(Planes(ps));
shadowpoints := ShadowOfElement(ps,plane,1);
List(shadowpoints);
shadowlines := ShadowOfElement(ps,plane,2);
List(shadowlines);
quit;
