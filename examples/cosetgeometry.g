## Coset geometry example: PG(3,q)

LoadPackage("grape");
pg := ProjectiveSpace(3, 3);
p := Random(Points(pg));
l := Random(Lines(p));
pi := Random(Planes(l));
pgl := ProjectivityGroup( pg );
stabp := Stabilizer(pgl, p, OnProjSubspaces);
stabl := Stabilizer(pgl, l, OnProjSubspaces);
stabpi := Stabilizer(pgl, pi, OnProjSubspaces);
hom := NiceMonomorphism(pgl);
impgl := Image(hom);
imstabp := Image(hom, stabp);
imstabl := Image(hom, stabl);
imstabpi := Image(hom, stabpi);
cg := CosetGeometry(impgl, [imstabp, imstabl, imstabpi]);
elms1 := AsList(ElementsOfIncidenceStructure(cg, 1));;
p := Random(elms1);
IsFlagTransitiveGeometry( cg );
res := ResidueOfFlag( cg, [p] );
gamma := IncidenceGraph( res );;
KnownAttributesOfObject( res );
Girth( gamma );
aut := AutGroupGraph( gamma );
DisplayCompositionSeries( aut );