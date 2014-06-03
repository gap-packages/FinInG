#example of using Random causing a NiceObject to be computed
pg := PG(4,8);
group := CollineationGroup(pg);
HasNiceMonomorphism(group);
Random(group);
time;
HasNiceMonomorphism(group);
Random(group);
time;
quit;

