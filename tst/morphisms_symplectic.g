q := 2;
a := 2;
n := 2;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));

q := 3;
a := 2;
n := 2;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));

q := 4;
a := 2;
n := 2;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));

q := 5;
a := 2;
n := 2;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));

q := 2;
a := 3;
n := 2;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));

q := 3;
a := 3;
n := 2;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));

q := 2;
a := 2;
n := 3;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
planes := AsList(Planes(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));
Set(List(planes,x->x^em in w2));


#more than 3G needed for the next test
q := 2;
a := 3;
n := 3;

w1 := SymplecticSpace(2*n-1,q^a);
w2 := SymplecticSpace(2*n*a-1,q);
pts := AsList(Points(w1));
lines := AsList(Lines(w1));
planes := AsList(Planes(w1));
em := NaturalEmbeddingByFieldReduction(w1,w2);
Set(List(pts,x->x^em in w2));
Set(List(lines,x->x^em in w2));
Set(List(planes,x->x^em in w2));

