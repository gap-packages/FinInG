## gpolygons_EGQByqClan.g
f := GF(3);
id := IdentityMat(2, f);;
list := List( f, t -> t * id );;
clan := qClan(list,f);
egq := EGQByqClan(clan);
incgraph := IncidenceGraphOfGeneralisedPolygon(egq);;
group := AutomorphismGroup(incgraph);
Order(group);
Order(CollineationGroup(HermitianVariety(3,9)));
quit;
