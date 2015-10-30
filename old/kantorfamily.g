## In this example, we construct the classical elation
## generalised quadrangle of order 3 by using a Kantor family.

g := ElementaryAbelianGroup(27);

## arc
subs := SubgroupsSolvableGroup( g );;
subs3 := Filtered(subs, x -> Size(x) = 3);;
subs9 := Filtered(subs, x -> Size(x) = 9);;

combs := Combinations(subs3, 4);;
f := First(combs, x -> ForAll(Combinations(x,3), y->
       IsTrivial(Intersection(DoubleCoset(y[1],One(g),y[2]), y[3]))));

## tangent spaces
fstar := List(f, x -> First(subs9, y -> IsSubgroup(y,x) and 
           ForAll(Difference(f,[x]), z->IsTrivial(Intersection(z,y)))));

## the kantor family is (g, f, fstar)
IsKantorFamily( g, f, fstar );
egq := EGQByKantorFamily(g, f, fstar);
coll := ElationGroup(egq); 
points := Points( egq );
p:=Random(points);
x:=Random(coll);
OnKantorFamily(p,x);
orbs := Orbits(coll, AsSet(points), OnKantorFamily);;
List(orbs,Size);


