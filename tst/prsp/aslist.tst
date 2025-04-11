gap> START_TEST("Forms: aslist.tst");
gap> pg := PG(3,5);
ProjectiveSpace(3, 5)
gap> lines := Lines(pg);
<lines of ProjectiveSpace(3, 5)>
gap> list := List(lines);;
gap> Length(list);
806
gap> lines := Lines(pg);
<lines of ProjectiveSpace(3, 5)>
gap> aslist := AsList(lines);;
gap> Length(aslist);
806
gap> STOP_TEST("aslist.tst", 10000 );
