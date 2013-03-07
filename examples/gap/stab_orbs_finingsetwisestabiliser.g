#finingsetwisestabiliser
ps := HyperbolicQuadric(5,5);                   
g := IsometryGroup(ps);
plane1 := Random(Planes(ps));
plane2 := Random(Planes(ps));
FiningSetwiseStabiliser(g,Set([plane1,plane2]));
quit;

