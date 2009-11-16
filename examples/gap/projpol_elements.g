#creating elements of a projective or polar spae.
ps := ProjectiveSpace(3,2);
v := [0,1,0,1]*Z(2)^0;
point := VectorSpaceToElement(ps,v);
ps := HyperbolicQuadric(5,7);
v := [Z(7)^0,Z(7)^0,Z(7)^2,Z(7),0*Z(7),0*Z(7)];
point := VectorSpaceToElement(ps,v);

ps := EllipticQuadric(3,27);
Display(ps);
ps := EllipticQuadric(5,8);
Display(ps);
quit;
