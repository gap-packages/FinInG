## gpolygons_EGQByKantorFamily.g
g := ElementaryAbelianGroup(27);
flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]),
            Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];;
IsKantorFamily( g, flist1, flist2 );
egq := EGQByKantorFamily(g, flist1, flist2);
CategoriesOfObject(egq);
p := Random(Points(egq));
CategoriesOfObject(p);
quit;
