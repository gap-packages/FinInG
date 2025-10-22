#Fining orbits test
pg := PG(2,16);
group := CollineationGroup(ParabolicQuadric(2,16));
orbs := FiningOrbits(group,Points(pg));
orbs := FiningOrbits(group,List(Points(pg)),OnProjSubspaces);
orbs := FiningOrbits(group,Lines(pg));
orbs := FiningOrbits(group,List(Lines(pg)),OnProjSubspaces);
pg := PG(3,9);
group := CollineationGroup(EllipticQuadric(3,9));
orbs := FiningOrbits(group,Points(pg));
orbs := FiningOrbits(group,List(Points(pg)),OnProjSubspaces);
orbs := FiningOrbits(group,Lines(pg));
orbs := FiningOrbits(group,List(Lines(pg)),OnProjSubspaces);
orbs := FiningOrbits(group,Planes(pg));
orbs := FiningOrbits(group,List(Planes(pg)),OnProjSubspaces);
pg := PG(2,49);
group := CollineationGroup(HermitianPolarSpace(2,49));
orbs := FiningOrbits(group,Points(pg));
orbs := FiningOrbits(group,List(Points(pg)),OnProjSubspaces);
orbs := FiningOrbits(group,Lines(pg));
orbs := FiningOrbits(group,List(Lines(pg)),OnProjSubspaces);
quit;

