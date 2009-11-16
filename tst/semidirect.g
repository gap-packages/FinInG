# Shows how to build a "semilinear" group using standard library things
# Unfortunately, it does not work well for a little bit bigger groups!
p := 3;
k := 2;  # must be even
q := p^k;
f := GF(p,k);
d := 2;
frob := FrobeniusAutomorphism(f);
ourfrob := frob^(k/2);
ourfrobinv := ourfrob^-1;

OurFrobForMatrices := function(m)
  m := List(m,v->List(v,x->x^ourfrob));
  ConvertToMatrixRep(m,q);
  return m;
end;

OurInverseFrobForMatrices := function(m)
  m := List(m,v->List(v,x->x^ourfrobinv));
  ConvertToMatrixRep(m,q);
  return m;
end;

g := GL(d,q);
a := GroupHomomorphismByFunction(g,g,
          OurFrobForMatrices,OurInverseFrobForMatrices);
aa := Group(a);
IsGroupOfAutomorphisms(aa);  # this is important

s := SemidirectProduct(aa,g);



