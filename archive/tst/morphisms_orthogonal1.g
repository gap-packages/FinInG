#no bugs in the below executed tests.
q := 2;
a := 2;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 3;
a := 2;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 4;
a := 2;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 5;
a := 2;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 7;
a := 2;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 2;
a := 3;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 3;
a := 3;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 4;
a := 3;
n := 2;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 2;
a := 2;
n := 3;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
planes := AsList(Planes(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));
Set(List(planes,x->x^em in ps2));

q := 2;
a := 3;
n := 3;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
planes := AsList(Planes(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));
Set(List(planes,x->x^em in ps2));

q := 2;
a := 2;
n := 4;

ps1 := HyperbolicQuadric(2*n-1,q^a);
ps2 := HyperbolicQuadric(2*n*a-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
planes := AsList(Planes(ps1));
solids := AsList(Solids(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));
Set(List(planes,x->x^em in ps2));
Set(List(solids,x->x^em in ps2));

