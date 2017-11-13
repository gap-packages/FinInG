gap> START_TEST("fining: tst_finingorbits.tst");
gap> #test FiningOrbits functions.
gap> pg := PG(2,16);
ProjectiveSpace(2, 16)
gap> group := CollineationGroup(ParabolicQuadric(2,16));
PGammaO(3,16)
gap> orbs := FiningOrbits(group,Points(pg));
93%..100%..[ <closed orbit, 1 points>, <closed orbit, 255 points>, 
  <closed orbit, 17 points> ]
gap> orbs := FiningOrbits(group,List(Points(pg)),OnProjSubspaces);
93%..100%..[ <closed orbit, 1 points>, <closed orbit, 255 points>, 
  <closed orbit, 17 points> ]
gap> orbs := FiningOrbits(group,Lines(pg));
6%..56%..100%..[ <closed orbit, 17 points>, <closed orbit, 136 points>, 
  <closed orbit, 120 points> ]
gap> orbs := FiningOrbits(group,List(Lines(pg)),OnProjSubspaces);
6%..56%..100%..[ <closed orbit, 17 points>, <closed orbit, 136 points>, 
  <closed orbit, 120 points> ]
gap> pg := PG(3,9);
ProjectiveSpace(3, 9)
gap> group := CollineationGroup(EllipticQuadric(3,9));
PGammaO-(4,9)
gap> orbs := FiningOrbits(group,Points(pg));
90%..100%..[ <closed orbit, 738 points>, <closed orbit, 82 points> ]
gap> orbs := FiningOrbits(group,List(Points(pg)),OnProjSubspaces);
90%..100%..[ <closed orbit, 738 points>, <closed orbit, 82 points> ]
gap> orbs := FiningOrbits(group,Lines(pg));
44%..89%..100%..[ <closed orbit, 3321 points>, <closed orbit, 3321 points>, 
  <closed orbit, 820 points> ]
gap> orbs := FiningOrbits(group,List(Lines(pg)),OnProjSubspaces);
44%..89%..100%..[ <closed orbit, 3321 points>, <closed orbit, 3321 points>, 
  <closed orbit, 820 points> ]
gap> orbs := FiningOrbits(group,Planes(pg));
10%..100%..[ <closed orbit, 82 points>, <closed orbit, 738 points> ]
gap> orbs := FiningOrbits(group,List(Planes(pg)),OnProjSubspaces);
10%..100%..[ <closed orbit, 82 points>, <closed orbit, 738 points> ]
gap> pg := PG(2,49);
ProjectiveSpace(2, 49)
gap> group := CollineationGroup(HermitianPolarSpace(2,49));
PGammaU(3,7^2)
gap> orbs := FiningOrbits(group,Points(pg));
85%..100%..[ <closed orbit, 2107 points>, <closed orbit, 344 points> ]
gap> orbs := FiningOrbits(group,List(Points(pg)),OnProjSubspaces);
85%..100%..[ <closed orbit, 2107 points>, <closed orbit, 344 points> ]
gap> orbs := FiningOrbits(group,Lines(pg));
85%..100%..[ <closed orbit, 2107 points>, <closed orbit, 344 points> ]
gap> orbs := FiningOrbits(group,List(Lines(pg)),OnProjSubspaces);
85%..100%..[ <closed orbit, 2107 points>, <closed orbit, 344 points> ]
gap> STOP_TEST("tst_finingorbits.tst", 10000 );
