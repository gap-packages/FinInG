#EGQ and all around 
f := GF(3);
id := IdentityMat(2, f);;
clan := List( f, t -> t * id );;
IsqClan( clan, f );
f := GF(7);
id := IdentityMat(2, f);;
list := List( f, t -> t * id );;
clan := qClan(list,f);
fam := KantorFamilyByqClan(clan);
egq := EGQByKantorFamily(fam[1],fam[2],fam[3]);
clan := KantorKnuthqClan(9);
blt := BLTSetByqClan(clan);
egq := EGQByBLTSet(blt);
DefiningPlanesOfEGQByBLTSet(egq);
clan := FisherThasWalkerKantorBettenqClan(11);
blt := BLTSetByqClan(clan);
clan := LinearqClan(3);
bltset := BLTSetByqClan( clan);
egq := EGQByBLTSet( bltset );
p := BasePointOfEGQ(egq);
UnderlyingObject(p);
group := ElationGroup(egq);
Order(group);
CollineationGroup(egq);
egq := EGQByqClan(clan);
CollineationGroup(egq);
g := ElementaryAbelianGroup(27);
flist1 := [ Group(g.1), Group(g.2), Group(g.3), Group(g.1*g.2*g.3) ];;
flist2 := [ Group([g.1, g.2^2*g.3]), Group([g.2, g.1^2*g.3 ]), 
            Group([g.3, g.1^2*g.2]), Group([g.1^2*g.2, g.1^2*g.3 ]) ];; 
IsKantorFamily( g, flist1, flist2 );
egq := EGQByKantorFamily(g, flist1, flist2);
CategoriesOfObject(egq);
p := Random(Points(egq));
CategoriesOfObject(p);
group := ElationGroup(egq);
CollineationAction(group) = OnKantorFamily;
l := ObjectToElement(egq,RightCoset(flist1[1],One(g)));
stab := Stabilizer(group,l,OnKantorFamily);
pts := List(Points(egq));
Orbits(group,pts,OnKantorFamily);
lines := List(Lines(egq));
Orbits(group,lines,OnKantorFamily);
quit;
