#list and aslist
pg := PG(3,4);
lines := Lines(pg);
list := List(lines);;
Length(list);
aslist := AsList(lines);
list2 := List(aslist);;
Length(list2);
quit;
