# distance between elements
g := ElementaryAbelianGroup(27);
flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]),
            Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];;
egq := EGQByKantorFamily(g, flist1, flist2);
p := Random(Points(egq));
q := Random(Points(egq));
DistanceBetweenElements(p,q);
gh := SplitCayleyHexagon(3);
l := Random(Lines(gh));
m := First(Lines(gh),x->DistanceBetweenElements(l,x)=6);
quit;

