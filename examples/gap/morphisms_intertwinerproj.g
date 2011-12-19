#intertwiner for morphisms for projective spaces
pg1 := PG(1,9);
em := NaturalEmbeddingByFieldReduction(pg1,GF(3));
i := Intertwiner(em);
spread := List(Points(pg1),x->x^em);
stab := Stabilizer(CollineationGroup(PG(3,3)),Set(spread),OnSets);
hom := HomographyGroup(pg1);
gens := GeneratorsOfGroup(hom);;
group := Group(List(gens,x->x^i));
Order(group);
IsSubgroup(stab,group);
quit;
