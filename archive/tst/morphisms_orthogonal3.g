q := 5;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 9;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 13;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 17;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));    #bug with AsList
pts := Points(ps1);
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 25;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));    #bug with AsList
pts := Points(ps1);
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));




q := 5;
a := 1;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 3;
a := 2;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 4;
a := 2;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 5;
a := 2;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 7;
a := 2;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 8;
a := 2;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 9;
a := 2;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 2;
a := 3;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 3;
a := 3;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

#more than 4G needed.
q := 4;
a := 3;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

#bug here:
q := 2;
a := 4;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

#bug here:
q := 2;
a := 5;
n := 2;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

#now with lines

q := 2;
a := 2;
n := 3;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 3;
a := 2;
n := 3;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 2;
a := 3;
n := 3;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

q := 2;
a := 4;
n := 3;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));

#now with planes
q := 2;
a := 2;
n := 4;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := HyperbolicQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
lines := AsList(Lines(ps1));
planes := AsList(Planes(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));
Set(List(lines,x->x^em in ps2));
Set(List(planes,x->x^em in ps2));



