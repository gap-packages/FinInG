## In this example, we construct all of the spreads
## of W(5,3) which admit a group with order divisible
## by 13.

w := SymplecticSpace(5, 3);
g := IsometryGroup(w);
syl := SylowSubgroup(g, 13);
planes := AsList(Planes( w ));;
points := AsList(Points( w ));;
orbs := Orbits(syl, planes, OnProjSubspaces);;

IsPartialSpread := x -> Number(points, p -> ForAny(x, i-> p in i)) = Size(x)*13;;
partialspreads := Filtered(orbs, IsPartialSpread);;
13s := Filtered(partialspreads, i -> Size(i) = 13);;
26s := List(Combinations(13s,2), Union);;
two := Union(Filtered(partialspreads, i -> Size(i) = 1));;
good26s := Filtered(26s, x->IsPartialSpread(Union(x, two)));;
spreads := List(good26s, x->Union(x, two));;

## There will be 5 spreads: two Hering spreads, two Albert twisted field, 
## and the regular spread.

for s in spreads do
  stab := SetwiseStabilizer(g, OnProjSubspaces, s)!.setstab;
  Print(Size(stab),"\n");
od;