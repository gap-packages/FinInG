#field reduction for polar spaces (1)
ps1 := SymplecticSpace(1,3^3);
em := NaturalEmbeddingByFieldReduction(ps1,GF(3),true);
ps2 := AmbientGeometry(Range(em));
spread := List(Points(ps1),x->x^em);;
i := Intertwiner(em);
coll := CollineationGroup(ps2);
stab := Group(ImagesSet(i,GeneratorsOfGroup(IsometryGroup(ps1))));
IsSubgroup(coll,stab);
List(Orbit(stab,spread[1]),x->x in spread);
quit;
