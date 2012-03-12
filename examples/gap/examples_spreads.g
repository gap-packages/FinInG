# examples_spreads.g
w := SymplecticSpace(5, 3);
g := IsometryGroup(w);
syl := SylowSubgroup(g, 13);
planes := Planes( w );
points := Points( w );
orbs := Orbits(syl, planes , OnProjSubspaces);;
IsPartialSpread := x -> Number(points, p ->
         ForAny(x, i-> p in i)) = Size(x)*13;;
partialspreads := Filtered(orbs, IsPartialSpread);;
Length(partialspreads);
13s := Filtered(partialspreads, i -> Size(i) = 13);;
Length(13s);
13s[1];
26s := List(Combinations(13s,2), Union);;
two := Union(Filtered(partialspreads, i -> Size(i) = 1));;
good26s := Filtered(26s, x->IsPartialSpread(Union(x, two)));;
spreads := List(good26s, x->Union(x, two));;
Length(spreads);
quit;
