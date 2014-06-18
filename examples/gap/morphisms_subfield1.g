#morphisms_subfield1.g
pg1 := PG(2,3);
pg2 := PG(2,9);
em := NaturalEmbeddingBySubfield(pg1,pg2);
points := AsList(Points( pg1 ));
image := ImagesSet(em, points);
hom := Intertwiner(em);
group1 := ProjectivityGroup(pg1);
gens := GeneratorsOfGroup(group1);
group1_image := Group(List(gens,x->x^hom));
Order(group1_image);
group2 := ProjectivityGroup(pg2);
Order(group2);
g := Random(group2);
PreImageElm(hom,g);
quit;
