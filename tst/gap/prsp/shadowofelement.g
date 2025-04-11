#ShadowOfElement
ps := PG(4,3);
plane := VectorSpaceToElement(ps,[[1,1,0,0,0],[1,0,1,0,0],[0,0,0,0,1]]*Z(3)^0);
shadowpoints := ShadowOfElement(ps,plane,1);
List(shadowpoints);
shadowlines := ShadowOfElement(ps,plane,2);
List(shadowlines);
quit;
