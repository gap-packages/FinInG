#objects/elements of EGQ
g := ElementaryAbelianGroup(27);
flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]),
            Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];;
egq := EGQByKantorFamily(g, flist1, flist2);
h := Random(g);
p := ObjectToElement(egq,h);
coset := RightCoset(flist1[1],h);
l := ObjectToElement(egq,coset);
p * l;
S := flist2[2];
m := ObjectToElement(egq,S);
q := BasePointOfEGQ(egq);
m * q;
lines := List(Lines(p));
pts1 := List(Points(m));
pts2 := List(Points(l));
List(pts2,x->UnderlyingObject(x));
UnderlyingObject(q);
quit;

