#flags of a projective space.
ps := ProjectiveSpace(12,11);
s1 := RandomSubspace(ps,8);
s2 := RandomSubspace(s1,6);
s3 := RandomSubspace(s2,4);
s4 := Random(Solids(s3));
s5 := Random(Points(s4));
flag := FlagOfIncidenceStructure(ps,[s1,s3,s2,s5,s4]);
ps := PG(4,5);
p := Random(Points(ps));
l := Random(Lines(ps));
v := Random(Solids(ps));
flag := FlagOfIncidenceStructure(ps,[v,l,p]);
quit;
flag := FlagOfIncidenceStructure(ps,[]);
quit;
