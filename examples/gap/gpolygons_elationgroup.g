#elation group of an EGQ by Kantor
g := ElementaryAbelianGroup(27);
flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]),
            Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];;
egq := EGQByKantorFamily(g, flist1, flist2);
group := ElationGroup(egq);
CollineationAction(group) = OnKantorFamily;
l := ObjectToElement(egq,RightCoset(flist1[1],One(g)));
stab := Stabilizer(group,l,OnKantorFamily);
pts := List(Points(egq));
Orbits(group,pts,OnKantorFamily);
lines := List(Lines(egq));
Orbits(group,lines,OnKantorFamily);
quit;
