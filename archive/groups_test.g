gapPGL := PGL(6,8);
finingPGL := HomographyGroup(PG(5,8));
o := Order(finingPGL);
Factors(o);
HasNiceObject(finingPGL);
finingsyl73:=SylowSubgroup(finingPGL,73);
time;
HasNiceObject(finingPGL);
gapsyl73:=SylowSubgroup(gapPGL,73);
time;
HasNiceObject(finingPGL);
finingsyl151:=SylowSubgroup(finingPGL,151);
time;
gapsyl151:=SylowSubgroup(gapPGL,151);
time;
finingsyl7:=SylowSubgroup(finingPGL,7);
time;
gapsyl7:=SylowSubgroup(gapPGL,7);
time;

# next example: first compute niceobject.

gapPGL := PGL(4,81);
finingPGL := HomographyGroup(PG(3,81));
o := Order(finingPGL);
Factors(o);
finingsyl41:=SylowSubgroup(finingPGL,41);
time;
gapsyl41:=SylowSubgroup(gapPGL,41);
time;

### ORder en NIceMonomorphism

group := CollineationGroup(PG(5,8));
HasNiceMonomorphism(group);
false
g := Random(group);
HasNiceMonomorphism(group);
h := Random(group);
i := Random(group);
G := Group([g,h,i]);
Order(G); #about 40 s
G := Group([g,h,i]);
SetParent(G,group);
Order(G); # about 2 s


