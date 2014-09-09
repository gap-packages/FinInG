## gpolygons_EGQByqClan.g
f := GF(3);
id := IdentityMat(2, f);;
list := List( f, t -> t * id );;
clan := qClan(list,f);
egq := EGQByqClan(clan);
incgraph := IncidenceGraph(egq);;
group := AutomorphismGroup(incgraph);
Order(group);
Order(CollineationGroup(HermitianPolarSpace(3,9)));
clan := KantorKnuthqClan(9);
egq := EGQByqClan(clan);
clan := FisherThasWalkerKantorBettenqClan(11);
quit;
