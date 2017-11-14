#Elements incident with element of incidence structure.
ps := HyperbolicQuadric(11,2);
vec := [[Z(2)^0,0*Z(2),0*Z(2),0*Z(2),0*Z(2),Z(2)^0,0*Z(2),Z(2)^0,Z(2)^0,
0*Z(2),0*Z(2),0*Z(2)],
[0*Z(2),Z(2)^0,0*Z(2),0*Z(2),0*Z(2),Z(2)^0,Z(2)^0,Z(2)^0,Z(2)^0,
0*Z(2),Z(2)^0,Z(2)^0],
[0*Z(2),0*Z(2),Z(2)^0,0*Z(2),0*Z(2),Z(2)^0,0*Z(2),Z(2)^0,0*Z(2),
0*Z(2),0*Z(2),Z(2)^0],
[0*Z(2),0*Z(2),0*Z(2),Z(2)^0,0*Z(2),Z(2)^0,0*Z(2),Z(2)^0,0*Z(2),
0*Z(2),Z(2)^0,0*Z(2)],
[0*Z(2),0*Z(2),0*Z(2),0*Z(2),Z(2)^0,Z(2)^0,0*Z(2),0*Z(2),0*Z(2),
Z(2)^0,Z(2)^0,Z(2)^0]];
subspace := VectorSpaceToElement(ps,vec);
els := ElementsIncidentWithElementOfIncidenceStructure(subspace,6);
List(els);
quit;
