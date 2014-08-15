## EGQ from blt set
clan := LinearqClan(3);
bltset := BLTSetByqClan( clan);
egq := EGQByBLTSet( bltset );
p := BasePointOfEGQ(egq);
UnderlyingObject(p);
l := Random(Lines(egq));
UnderlyingObject(l);
group := ElationGroup(egq);
Order(group);
CollineationGroup(egq);
time;
egq := EGQByqClan(clan);
CollineationGroup(egq);
time;
quit;
