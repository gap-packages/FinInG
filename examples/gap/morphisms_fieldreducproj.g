#morphisms by field reduction for projective spaces
pg1 := ProjectiveSpace(2,81);
f2 := GF(9);
em := NaturalEmbeddingByFieldReduction(pg1,f2);
f2 := GF(3);
em := NaturalEmbeddingByFieldReduction(pg1,f2);
pg2 := ProjectiveSpace(11,3);
em := NaturalEmbeddingByFieldReduction(pg1,pg2);
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
