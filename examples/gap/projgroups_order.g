#order of a collineation
x := Random(CollineationGroup(PG(4,9)));
t := Order(x);
IsOne(x^t);
quit;
