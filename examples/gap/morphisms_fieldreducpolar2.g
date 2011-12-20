#field reduction for polar spaces
ps1 := SymplecticSpace(3,9);
em := NaturalEmbeddingByFieldReduction(ps1,GF(3),true);
ps2 := AmbientGeometry(Range(em));
pg := AmbientSpace(ps2);
spread := List(Points(ps1),x->x^em);;
el := Random(ElementsOfIncidenceStructure(pg,5));
prebs := Filtered(spread,x->Meet(x,el) <> EmptySubspace(pg));;
bs := List(prebs,x->PreImageElm(em,x));;
Length(bs);
lines := List(Lines(ps1));;
Collected(List(lines,x->Length(Filtered(bs,y->y * x))));
quit;
