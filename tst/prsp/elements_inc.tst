gap> START_TEST("Forms: elements_inc.tst");
gap> ps := ProjectiveSpace(6,7);
ProjectiveSpace(6, 7)
gap> ElementsOfIncidenceStructure(ps,1);
<points of ProjectiveSpace(6, 7)>
gap> ElementsOfIncidenceStructure(ps,2);
<lines of ProjectiveSpace(6, 7)>
gap> ElementsOfIncidenceStructure(ps,3);
<planes of ProjectiveSpace(6, 7)>
gap> ElementsOfIncidenceStructure(ps,4);
<solids of ProjectiveSpace(6, 7)>
gap> ElementsOfIncidenceStructure(ps,5);
<proj. 4-subspaces of ProjectiveSpace(6, 7)>
gap> ElementsOfIncidenceStructure(ps,6);
<proj. 5-subspaces of ProjectiveSpace(6, 7)>
gap> STOP_TEST("elements_inc.tst", 10000 );
