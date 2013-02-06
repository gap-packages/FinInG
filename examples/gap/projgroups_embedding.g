#demonstrate embedding of collineation group into correlation group
coll := CollineationGroup(PG(4,8));
corr := CorrelationCollineationGroup(PG(4,8));
phi := Embedding(coll,corr);
quit;
