# hermitian spread of H(q)
gh := SplitCayleyHexagon(3);
q6 := AmbientPolarSpace(gh);
hyp := First(Hyperplanes(PG(6,3)),x->TypeOfSubspace(q6,x)="elliptic");
q5 := EllipticQuadric(5,3);
lines := AsList(Lines(q5));
em := NaturalEmbeddingBySubspace(q5,q6,hyp);
spread := Filtered(lines,x->x^em in gh);
spread := List(spread,x->ElementToElement(gh,x^em));
Collected(Concatenation(List(spread,x->List(spread,y->DistanceBetweenElements(x,y)))));
quit;
