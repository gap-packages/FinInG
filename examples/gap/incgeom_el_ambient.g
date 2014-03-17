#ambient geometry of an element
plane := Random(Planes(HyperbolicQuadric(5,2)));
AmbientGeometry(plane);
l := Random(Lines(SplitCayleyHexagon(3)));
Print(l);
AmbientGeometry(l);
p := Random(Points(EGQByBLTSet(BLTSetByqClan(LinearqClan(3)))));
Print(p);
AmbientGeometry(p);
quit;
