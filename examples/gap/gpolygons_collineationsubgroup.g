# collineation subgroup for GPS
clan := FisherThasWalkerKantorBettenqClan(5);
blt := BLTSetByqClan(clan);
egq := EGQByBLTSet(blt);
coll := CollineationSubgroup(egq);
Order(coll);
act := CollineationAction(coll);
orbs := Orbits(coll,Points(egq),act);;
List(orbs,x->Length(x));
el := ElationGroup(egq);
orbs := Orbits(el,Points(egq),act);;
List(orbs,x->Length(x));
quit;

