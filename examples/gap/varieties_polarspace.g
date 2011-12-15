#polar space from variety
f := GF(25);
r := PolynomialRing(f,4);
ind := IndeterminatesOfPolynomialRing(r);
eq1 := Sum(List(ind,t->t^2));
var := ProjectiveVariety(PG(3,f),[eq1]);   
PolarSpace(var);
eq2 := Sum(List(ind,t->t^4));
var := ProjectiveVariety(PG(3,f),[eq2]);
PolarSpace(var);
quit;
eq3 := Sum(List(ind,t->t^6));
var := ProjectiveVariety(PG(3,f),[eq3]);
PolarSpace(var);
quit;

