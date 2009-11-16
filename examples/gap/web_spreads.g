# spreads of W(5,3)
w := SymplecticSpace(5,3);
g := IsometryGroup(w);
syl := SylowSubgroup(g,13);
planes := Planes(w);
points := Points(w);
orbs := Orbits(syl,planes,OnPoints);;
IsPartialSpread := x -> Number(points,p -> ForAny(x,i -> p in i)) = Size(x)*13;;
partialspreads := Filtered(orbs,IsPartialSpread);;
13s := Filtered(partialspreads,i -> Size(i)=13);;
26s := List(Combinations(13s,2),Union);;
two := Union(Filtered(partialspreads,i -> Size(i)=1));;
good26s := Filtered(26s,x->IsPartialSpread(Union(x,two)));;
spreads := List(good26s,x -> Union(x,two));;
spreads[1];
quit;
