q := 3;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 7;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 11;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 19;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));    #bug with AsList 
pts := List(Points(ps1));;
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 23;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));    #bug with AsList 
pts := List(Points(ps1));;
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));


q := 31;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));    #bug with AsList 
pts := List(Points(ps1));;
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 43;
a := 1;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));    #bug with AsList 
pts := List(Points(ps1));;
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

######### a = 2;
bug for this value:

q := 3;
a := 2;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 7;
a := 2;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
#pts := AsList(Points(ps1));
pts := List(Points(ps1));;
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));

q := 5;
a := 2;
n := 1;

ps1 := ParabolicQuadric(2*n,q^(2*a));
ps2 := EllipticQuadric(2*a*(2*n+1)-1,q);
pts := AsList(Points(ps1));
em := NaturalEmbeddingByFieldReduction(ps1,ps2,false);
Set(List(pts,x->x^em in ps2));


